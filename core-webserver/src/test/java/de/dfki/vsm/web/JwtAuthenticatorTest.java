package de.dfki.vsm.web;

import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jose.crypto.RSASSASigner;
import com.nimbusds.jose.jwk.RSAKey;
import com.nimbusds.jose.jwk.gen.RSAKeyGenerator;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.SignedJWT;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Exercises {@link JwtAuthenticator} against a real (if throwaway) JWKS served over HTTP,
 * signing real JWTs with a generated RSA key — not mocked, so this covers the actual
 * signature-verification and claims-checking path a real Keycloak-issued token goes through.
 */
class JwtAuthenticatorTest {

    private static final String ISSUER = "https://test-issuer.example/realms/test";

    private HttpServer jwksServer;
    private String jwksUrl;
    private RSAKey rsaKey;

    @BeforeEach
    void setUp() throws Exception {
        rsaKey = new RSAKeyGenerator(2048).keyID("test-key-1").generate();
        String jwkSetJson = new com.nimbusds.jose.jwk.JWKSet(rsaKey.toPublicJWK()).toString();

        jwksServer = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        jwksServer.createContext("/certs", exchange -> {
            byte[] body = jwkSetJson.getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        jwksServer.start();
        jwksUrl = "http://127.0.0.1:" + jwksServer.getAddress().getPort() + "/certs";
    }

    @AfterEach
    void tearDown() {
        jwksServer.stop(0);
    }

    private String signedJwt(String issuer, String subject, Date expiration) throws Exception {
        JWTClaimsSet claims = new JWTClaimsSet.Builder()
                .issuer(issuer)
                .subject(subject)
                .claim("preferred_username", subject + "@example.dfki.de")
                .expirationTime(expiration)
                .build();
        SignedJWT jwt = new SignedJWT(
                new JWSHeader.Builder(JWSAlgorithm.RS256).keyID(rsaKey.getKeyID()).build(), claims);
        jwt.sign(new RSASSASigner(rsaKey));
        return jwt.serialize();
    }

    @Test
    void disabledWhenIssuerNotConfigured() {
        JwtAuthenticator auth = new JwtAuthenticator(null, null, null);
        assertFalse(auth.isEnabled());
        assertNull(auth.verify("anything"));
    }

    @Test
    void acceptsValidToken() throws Exception {
        JwtAuthenticator auth = new JwtAuthenticator(ISSUER, null, jwksUrl);
        assertTrue(auth.isEnabled());
        String jwt = signedJwt(ISSUER, "alice", new Date(System.currentTimeMillis() + 60_000));

        JWTClaimsSet claims = auth.verify(jwt);

        assertNotNull(claims);
        assertEquals("alice", claims.getSubject());
        assertEquals("alice@example.dfki.de", claims.getStringClaim("preferred_username"));
    }

    @Test
    void rejectsExpiredToken() throws Exception {
        JwtAuthenticator auth = new JwtAuthenticator(ISSUER, null, jwksUrl);
        String expired = signedJwt(ISSUER, "alice", new Date(System.currentTimeMillis() - 60_000));

        assertNull(auth.verify(expired));
    }

    @Test
    void rejectsWrongIssuer() throws Exception {
        JwtAuthenticator auth = new JwtAuthenticator(ISSUER, null, jwksUrl);
        String wrongIssuer = signedJwt("https://not-the-configured-issuer.example",
                "alice", new Date(System.currentTimeMillis() + 60_000));

        assertNull(auth.verify(wrongIssuer));
    }

    @Test
    void rejectsTokenSignedByAnUntrustedKey() throws Exception {
        JwtAuthenticator auth = new JwtAuthenticator(ISSUER, null, jwksUrl);
        RSAKey otherKey = new RSAKeyGenerator(2048).keyID("test-key-1").generate();
        JWTClaimsSet claims = new JWTClaimsSet.Builder()
                .issuer(ISSUER).subject("mallory")
                .expirationTime(new Date(System.currentTimeMillis() + 60_000)).build();
        SignedJWT forged = new SignedJWT(
                new JWSHeader.Builder(JWSAlgorithm.RS256).keyID(otherKey.getKeyID()).build(), claims);
        forged.sign(new RSASSASigner(otherKey));

        assertNull(auth.verify(forged.serialize()));
    }

    @Test
    void enforcesAudienceWhenConfigured() throws Exception {
        JwtAuthenticator auth = new JwtAuthenticator(ISSUER, "vsm-frontend", jwksUrl);
        String noAudience = signedJwt(ISSUER, "alice", new Date(System.currentTimeMillis() + 60_000));

        assertNull(auth.verify(noAudience));
    }

    @Test
    void rejectsMissingOrBlankToken() {
        JwtAuthenticator auth = new JwtAuthenticator(ISSUER, null, jwksUrl);
        assertNull(auth.verify(null));
        assertNull(auth.verify(""));
        assertNull(auth.verify("not-a-jwt"));
    }
}

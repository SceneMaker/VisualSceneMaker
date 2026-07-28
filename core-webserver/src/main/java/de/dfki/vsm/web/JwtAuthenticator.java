package de.dfki.vsm.web;

import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.jwk.source.JWKSource;
import com.nimbusds.jose.jwk.source.JWKSourceBuilder;
import com.nimbusds.jose.proc.JWSVerificationKeySelector;
import com.nimbusds.jose.proc.SecurityContext;
import com.nimbusds.jwt.JWTClaimsSet;
import com.nimbusds.jwt.proc.ConfigurableJWTProcessor;
import com.nimbusds.jwt.proc.DefaultJWTClaimsVerifier;
import com.nimbusds.jwt.proc.DefaultJWTProcessor;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.net.URI;
import java.util.Set;

/**
 * Validates Bearer JWTs against a configured OIDC provider's JWKS (signature,
 * issuer, audience, expiry). A no-op unless {@code OIDC_ISSUER_URL} is set, so
 * desktop/single-user deployments that never configure OIDC are unaffected —
 * see doc/vsm-workspace-platform-plan.md Phase 1.
 *
 * <p>Deliberately does not touch {@link SessionGate} — callers resolve the
 * returned claims to a {@code UserToken} themselves. This class only answers
 * "is this JWT valid," nothing about sessions/roles.
 */
public class JwtAuthenticator {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    private final String mIssuer;
    private final ConfigurableJWTProcessor<SecurityContext> mProcessor;

    public JwtAuthenticator() {
        this(System.getenv("OIDC_ISSUER_URL"), System.getenv("OIDC_AUDIENCE"), System.getenv("OIDC_JWKS_URL"));
    }

    /** Package-private, exercised directly by tests without env vars. */
    JwtAuthenticator(String issuer, String audience, String jwksUrl) {
        if (issuer == null || issuer.isBlank()) {
            mIssuer = null;
            mProcessor = null;
            return;
        }
        mIssuer = issuer;
        String effectiveJwksUrl = (jwksUrl == null || jwksUrl.isBlank())
                ? issuer.replaceAll("/+$", "") + "/protocol/openid-connect/certs"
                : jwksUrl;
        try {
            JWKSource<SecurityContext> jwkSource =
                    JWKSourceBuilder.<SecurityContext>create(URI.create(effectiveJwksUrl).toURL()).build();
            DefaultJWTProcessor<SecurityContext> processor = new DefaultJWTProcessor<>();
            processor.setJWSKeySelector(new JWSVerificationKeySelector<>(JWSAlgorithm.RS256, jwkSource));
            JWTClaimsSet exactMatchClaims = new JWTClaimsSet.Builder().issuer(mIssuer).build();
            String effectiveAudience = (audience == null || audience.isBlank()) ? null : audience;
            processor.setJWTClaimsSetVerifier(
                    new DefaultJWTClaimsVerifier<>(effectiveAudience, exactMatchClaims, Set.of("sub", "exp")));
            mProcessor = processor;
            sLogger.message("OIDC auth enabled: issuer=" + mIssuer + " jwks=" + effectiveJwksUrl);
        } catch (Exception exc) {
            throw new IllegalStateException("Failed to initialize OIDC JWKS source from " + effectiveJwksUrl, exc);
        }
    }

    public boolean isEnabled() {
        return mProcessor != null;
    }

    /** {@code null} unless {@link #isEnabled()} — the OIDC issuer URL, for frontend discovery. */
    public String getIssuer() {
        return mIssuer;
    }

    /**
     * Verifies signature, issuer, audience (if configured), and expiry.
     * @return the verified claims, or {@code null} if the token is missing,
     *         malformed, or fails any check. Never throws.
     */
    public JWTClaimsSet verify(String bearerJwt) {
        if (!isEnabled() || bearerJwt == null || bearerJwt.isBlank()) {
            return null;
        }
        try {
            return mProcessor.process(bearerJwt, null);
        } catch (Exception exc) {
            sLogger.warning("JWT validation failed: " + exc.getMessage());
            return null;
        }
    }
}

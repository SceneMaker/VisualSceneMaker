package de.dfki.vsm.xtension.email;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.util.Properties;
import javax.mail.Authenticator;
import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.mail.PasswordAuthentication;
import javax.mail.MessagingException;

/**
 * @author Gregor Mehlmann
 */
public final class GMailExecutor extends ActivityExecutor {

    private final String mSender;
    private final String mTarget;
    private final GmailAuthenticator mAuthent;

    // Construct executor
    public GMailExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
        // Get the config
        mSender = mConfig.getProperty("sender");
        mTarget = mConfig.getProperty("target");
        //
        final String userName = mConfig.getProperty("username");
        final String passWord = mConfig.getProperty("password");
        mAuthent = new GmailAuthenticator(userName, passWord);
    }

    // Get marker syntax
    @Override
    public synchronized String marker(final long id) {
        return "$(" + id + ")";
    }

    // Launch the executor
    @Override
    public void launch() {

    }

    // Unload the executor
    @Override
    public void unload() {

    }

    private String clip(final String input) {
        if (input != null) {
            return input.replace("'", "");
        }
        return null;
    }

    // Execute activity
    @Override
    public void execute(final AbstractActivity activity) {
        if (activity instanceof ActionActivity) {
            activity.setType(AbstractActivity.Type.parallel);
            final String target = clip(activity.get("target"));
            final String subject = clip(activity.get("subject"));
            final String content = clip(activity.get("content"));
            // Send the email
            send(target, subject, content);
        }
    }

    // Authentication
    private class GmailAuthenticator extends Authenticator {

        private final String mUserName;
        private final String mPassWord;

        public GmailAuthenticator(
                final String userName,
                final String passWord) {
            mUserName = userName;
            mPassWord = passWord;
        }

        @Override
        public final PasswordAuthentication getPasswordAuthentication() {
            return new PasswordAuthentication(mUserName, mPassWord);
        }
    }

    // Send an email
    private void send(
            final String target,
            final String subject,
            final String content) {
        try {
            final Properties config = new Properties();
            config.put("mail.transport.protocol", "smtp");
            config.put("mail.smtp.starttls.enable", "true");
            config.put("mail.smtp.host", "smtp.gmail.com");
            config.put("mail.smtp.port", "587");
            config.put("mail.smtp.auth", "true");
            // Create the session
            final Session session = Session.getDefaultInstance(config, mAuthent);
            //session.setDebug(true);
            // Create the message
            final MimeMessage message = new MimeMessage(session);
            message.setSubject(subject, "UTF-8");
            message.setContent(content, "text/plain");
            message.setFrom(new InternetAddress(mSender));
            //
            if (target != null) {
                message.addRecipient(Message.RecipientType.TO, new InternetAddress(target));
            } else {
                message.addRecipient(Message.RecipientType.TO, new InternetAddress(mSender));
            }
            //
            message.addRecipients(Message.RecipientType.CC, mTarget);

            // Debug some message    
            System.out.println("Sending email with subject '" + subject + "' and content '" + content + "'");
            // Send the mesage
            Transport.send(message);
        } catch (final MessagingException exc) {
            mLogger.warning(exc.toString());
        }
    }
}

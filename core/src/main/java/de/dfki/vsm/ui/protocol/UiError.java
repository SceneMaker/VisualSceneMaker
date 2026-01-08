package de.dfki.vsm.ui.protocol;

import java.util.Collections;
import java.util.Map;

public final class UiError {
    private final String mCode;
    private final String mMessage;
    private final Map<String, Object> mDetails;
    private final boolean mRetryable;

    public UiError(final String code, final String message) {
        this(code, message, Collections.emptyMap(), false);
    }

    public UiError(final String code, final String message, final Map<String, Object> details, final boolean retryable) {
        mCode = code;
        mMessage = message;
        mDetails = details == null ? Collections.emptyMap() : details;
        mRetryable = retryable;
    }

    public String getCode() {
        return mCode;
    }

    public String getMessage() {
        return mMessage;
    }

    public Map<String, Object> getDetails() {
        return mDetails;
    }

    public boolean isRetryable() {
        return mRetryable;
    }
}

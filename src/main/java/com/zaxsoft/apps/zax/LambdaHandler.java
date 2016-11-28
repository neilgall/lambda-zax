package com.zaxsoft.apps.zax;

import java.util.HashSet;
import java.util.Set;

import com.amazon.speech.speechlet.lambda.SpeechletRequestStreamHandler;

public final class LambdaHandler extends SpeechletRequestStreamHandler {
    private static final Set<String> supportedApplicationIds = new HashSet<String>();
    static {
        supportedApplicationIds.add("amzn1.ask.skill.e268a389-b9b3-489d-bee4-f0c7e20e1cb5");
    }

    public LambdaHandler() {
        super(new ZaxSpeechlet(), supportedApplicationIds);
    }
}

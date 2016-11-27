package com.zaxsoft.apps.zax;

import java.util.HashSet;
import java.util.Set;

import com.amazon.speech.speechlet.lambda.SpeechletRequestStreamHandler;

public final class LambdaHandler extends SpeechletRequestStreamHandler {
    private static final Set<String> supportedApplicationIds = new HashSet<String>();
    static {
        supportedApplicationIds.add("amzn1.ask.skill.09a7d891-c6ed-44a9-9d8c-6ee6a053cab7");
    }

    public LambdaHandler() {
        super(new ZaxSpeechlet(), supportedApplicationIds);
    }
}

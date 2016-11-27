package com.zaxsoft.apps.zax;

import com.zaxsoft.streams.S3StreamProvider;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.amazon.speech.slu.Intent;
import com.amazon.speech.speechlet.IntentRequest;
import com.amazon.speech.speechlet.LaunchRequest;
import com.amazon.speech.speechlet.Session;
import com.amazon.speech.speechlet.SessionEndedRequest;
import com.amazon.speech.speechlet.SessionStartedRequest;
import com.amazon.speech.speechlet.Speechlet;
import com.amazon.speech.speechlet.SpeechletException;
import com.amazon.speech.speechlet.SpeechletResponse;
import com.amazon.speech.speechlet.User;
import com.amazon.speech.ui.PlainTextOutputSpeech;
import com.amazon.speech.ui.Reprompt;
import com.amazon.speech.ui.SimpleCard;

public class ZaxSpeechlet implements Speechlet {
    private static final Logger log = LoggerFactory.getLogger(ZaxSpeechlet.class);

    private Zax zaxForSession(final Session session) {
        final User user = session.getUser();
        final String userId = (user == null ? null : user.getUserId());
        final String prefix = (userId == null ? "default" : userId + "-");
        final S3StreamProvider streamProvider = new S3StreamProvider("alexa-zork", prefix);
        return new Zax("zork1.dat", streamProvider);
    }

    @Override
    public void onSessionStarted(final SessionStartedRequest request, final Session session)
            throws SpeechletException {
        log.info("onSessionStarted requestId={}, sessionId={} userId={}", request.getRequestId(),
                session.getSessionId(), session.getUser().getUserId());
    }

    @Override
    public SpeechletResponse onLaunch(final LaunchRequest request, final Session session)
            throws SpeechletException {
        log.info("onLaunch requestId={}, sessionId={} userId={}", request.getRequestId(),
                session.getSessionId(), session.getUser().getUserId());

        return getNewGameResponse(session);
    }

    @Override
    public SpeechletResponse onIntent(final IntentRequest request, final Session session)
            throws SpeechletException {
        log.info("onIntent requestId={}, sessionId={} userId={}", request.getRequestId(),
                session.getSessionId(), session.getUser().getUserId());

        Intent intent = request.getIntent();
        String intentName = (intent != null) ? intent.getName() : null;

        if ("NewGameIntent".equals(intentName)) {
            return getNewGameResponse(session);
        } else if ("AMAZON.HelpIntent".equals(intentName)) {
            return getHelpResponse();
        } else {
            throw new SpeechletException("Invalid Intent");
        }
    }

    @Override
    public void onSessionEnded(final SessionEndedRequest request, final Session session)
            throws SpeechletException {
        log.info("onSessionEnded requestId={}, sessionId={}", request.getRequestId(),
                session.getSessionId());
        // any cleanup logic goes here
    }

    private SpeechletResponse getNewGameResponse(final Session session) {
      final Zax zax = zaxForSession(session);
      zax.addCommand(ZaxCommand.recordingOutput("look"));
      zax.addCommand(ZaxCommand.ignoringOutput("quit"));
      zax.start();
      zax.runCommandQueue();

      final String output = zax.getOutput();
      log.debug("onLaunch finished: {}", output);

      return responseForZaxOutput(output);
    }

    /**
     * Creates and returns a {@code SpeechletResponse} with a welcome message.
     *
     * @return SpeechletResponse spoken and visual response for the given intent
     */
    private SpeechletResponse getWelcomeResponse() {
        String speechText = "Welcome to the Alexa Skills Kit, you can say hello";

        // Create the Simple card content.
        SimpleCard card = new SimpleCard();
        card.setTitle("HelloWorld");
        card.setContent(speechText);

        // Create the plain text output.
        PlainTextOutputSpeech speech = new PlainTextOutputSpeech();
        speech.setText(speechText);

        // Create reprompt
        Reprompt reprompt = new Reprompt();
        reprompt.setOutputSpeech(speech);

        return SpeechletResponse.newAskResponse(speech, reprompt, card);
    }

    /**
     * Build a SpeechletResponse from the Zax output
     */
    private SpeechletResponse responseForZaxOutput(final String output) {
      final PlainTextOutputSpeech speech = new PlainTextOutputSpeech();
      speech.setText(output);

      final Reprompt reprompt = new Reprompt();
      reprompt.setOutputSpeech(speech);

      final SimpleCard card = new SimpleCard();
      card.setTitle("Zork");
      card.setContent(output);

      return SpeechletResponse.newAskResponse(speech, reprompt, card);
    }

    /**
     * Creates a {@code SpeechletResponse} for the help intent.
     *
     * @return SpeechletResponse spoken and visual response for the given intent
     */
    private SpeechletResponse getHelpResponse() {
        String speechText = "You can say hello to me!";

        // Create the Simple card content.
        SimpleCard card = new SimpleCard();
        card.setTitle("HelloWorld");
        card.setContent(speechText);

        // Create the plain text output.
        PlainTextOutputSpeech speech = new PlainTextOutputSpeech();
        speech.setText(speechText);

        // Create reprompt
        Reprompt reprompt = new Reprompt();
        reprompt.setOutputSpeech(speech);

        return SpeechletResponse.newAskResponse(speech, reprompt, card);
    }
}

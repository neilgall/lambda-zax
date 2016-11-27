package com.zaxsoft.apps.zax;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.amazonaws.SdkClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.ObjectMetadata;

final class Pair<A,B> {
  public final A first;
  public final B second;
  public Pair(A a, B b) {
    first = a;
    second = b;
  }
}

final class S3StreamProvider implements StreamProvider {
  private static final Logger log = LoggerFactory.getLogger(S3StreamProvider.class);
  private final AmazonS3 S3;
  private final String bucketName;
  private final String filenamePrefix;
  private final List<Pair<String, ByteArrayOutputStream>> activeOutputStreams;

  public S3StreamProvider(final String bucketName, final String filenamePrefix) {
    S3 = new AmazonS3Client();
    this.bucketName = bucketName;
    this.filenamePrefix = filenamePrefix;
    activeOutputStreams = new ArrayList<>();
  }

  public InputStream getInputStream(final String name) {
    final String filename = filenamePrefix + name;
    try {
      final S3Object object = S3.getObject(bucketName, filename);
      if (object == null) {
        return null;
      }
      return object.getObjectContent();
    } catch (SdkClientException e) {
      log.error("S3 getObject({}, {}) failed: {}", bucketName, filename, e.toString());
      return null;
    }
  }

  public OutputStream getOutputStream(final String name) {
    final String filename = filenamePrefix + name;
    final ByteArrayOutputStream os = new ByteArrayOutputStream();
    activeOutputStreams.add(new Pair<>(filename, os));
    return os;
  }

  public void disposeInputStream(final InputStream is) {
    try {
      is.close();
    } catch (IOException e) {}
  }

  public void disposeOutputStream(final OutputStream os) {
    String filename = "?";
    try {
      final ByteArrayOutputStream baos = (ByteArrayOutputStream)os;
      filename = filenameForOutputStream(baos);
      final InputStream is = new ByteArrayInputStream(baos.toByteArray());
      final ObjectMetadata metadata = new ObjectMetadata();
      metadata.setContentLength(baos.size());
      S3.putObject(bucketName, filename, is, metadata);
    } catch (ClassCastException e) {
      log.error("Bad OutputStream type: {}", os);
    } catch (SdkClientException e) {
      log.error("S3 putObject({}, {}) failed: {}", bucketName, filename, e.toString());
    }
  }

  private String filenameForOutputStream(final ByteArrayOutputStream os) {
    for (Pair<String, ByteArrayOutputStream> pair : activeOutputStreams) {
      if (pair.second == os) {
        return pair.first;
      }
    }
    return null;
  }
}

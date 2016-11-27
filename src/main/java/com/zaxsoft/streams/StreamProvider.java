package com.zaxsoft.streams;

import java.io.InputStream;
import java.io.OutputStream;

public interface StreamProvider {
  InputStream getInputStream(final String name);
  OutputStream getOutputStream(final String name);
  void disposeInputStream(final InputStream is);
  void disposeOutputStream(final OutputStream os);
}

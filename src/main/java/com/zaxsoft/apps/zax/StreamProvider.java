package com.zaxsoft.apps.zax;

import java.io.InputStream;
import java.io.OutputStream;

interface StreamProvider {
  InputStream getInputStream(final String name);
  OutputStream getOutputStream(final String name);
  void disposeInputStream(final InputStream is);
  void disposeOutputStream(final OutputStream os);
}

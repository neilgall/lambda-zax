package com.zaxsoft.streams;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

public class FileStreamProvider implements StreamProvider {
  public InputStream getInputStream(final String name) {
    try {
      return new FileInputStream(name);
    } catch (IOException e) {
      return null;
    }
  }

  public OutputStream getOutputStream(final String name) {
    try {
      return new FileOutputStream(name);
    } catch (IOException e) {
      return null;
    }
  }

  public void disposeInputStream(final InputStream is) {
    try {
      is.close();
    } catch (IOException e) {}
  }

  public void disposeOutputStream(final OutputStream os) {
    try {
      os.close();
    } catch (IOException e) {}
  }

}

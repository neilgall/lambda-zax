/**
* Copyright (c) 2008 Matthew E. Kimmel
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
*/
package com.zaxsoft.apps.zax;

import com.zaxsoft.zmachine.ZCPU;
import com.zaxsoft.zmachine.ZUserInterface;
import com.zaxsoft.streams.StreamProvider;

import java.io.*;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.ArrayDeque;
import java.util.Queue;

/**
* Zax main class.
*
* @author Matt Kimmel
*/
class Zax implements ZUserInterface {
  static String versionString = "0.91";
  final String gameFile;
  ZCPU cpu;
  int version = 0;    // Version of this storyfile - 0 if game not yet initialized.
  int moreLines = 0; // Number of lines before next MORE
  Hashtable inputCharacters; // Used to translate between Event input characters and Z-Machine input characters
  Vector terminatingCharacters; // List of terminating characters for READ operations
  Thread cpuThread = null; // Thread of ZMachine CPU
  StreamProvider streamProvider;
  Queue<ZaxCommand> commandQueue;
  ZaxCommand currentCommand;
  StringBuilder output;
  String statusMessage = "";

  // Constructor
  public Zax(String gameFile, StreamProvider streamProvider)
  {
    this.gameFile = gameFile;
    this.streamProvider = streamProvider;
    this.output = new StringBuilder();
    this.commandQueue = new ArrayDeque<>();
  }

  public void addCommand(final ZaxCommand cmd)
  {
    commandQueue.add(cmd);
  }

  public void start()
  {
    cpu = new ZCPU(this);
    cpu.initialize(gameFile);
    cpuThread = cpu.start();
  }

  public void runCommandQueue() {
    while (!commandQueue.isEmpty()) {
      try {
        cpuThread.join();
      } catch (InterruptedException e) {}
    }
  }

  public String getOutput() {
    return output.toString();
  }

  /////////////////////////////////////////////////////////
  // ZUserInterface methods
  /////////////////////////////////////////////////////////

  // fatal - print a fatal error message and exit
  // Windows must be initialized!
  public void fatal(String errmsg)
  {
    System.out.println("FATAL ERROR: " + errmsg + "\n");
    System.exit(1);
  }

  // Initialize the user interface.  This consists of setting
  // up a status bar and a lower window in V1-2, and an upper
  // and lower window in V3-5,7-8.  Not sure yet what this
  // involves in V6.
  public void initialize(int ver)
  {
    version = ver;
  }

  // Sets the terminating characters for READ operations (other than
  // CR).  Translates from Z-Characters to Event characters by
  // enumerating through the inputCharacter table.
  public void setTerminatingCharacters(Vector chars)
  {
  }

  // We support a status line in V1-3 only.
  public boolean hasStatusLine()
  {
    return false;
  }

  // We support an upper window starting at V3.
  public boolean hasUpperWindow()
  {
    return false;
  }

  // For now, we always use a fixed-width font.
  public boolean defaultFontProportional()
  {
    return false;
  }

  public boolean hasFixedWidth()
  {
    return true;
  }

  // Yes, we have colors
  public boolean hasColors()
  {
    return false;
  }

  // Yes, we have italic
  public boolean hasItalic()
  {
    return false;
  }

  // Yes, we have boldface
  public boolean hasBoldface()
  {
    return false;
  }

  // Yes, we have timed input
  public boolean hasTimedInput()
  {
    return false;
  }

  // Our default background color is blue right now. FIX THIS
  public int getDefaultBackground()
  {
    return 6;
  }

  // Our default foreground color is white for now
  public int getDefaultForeground()
  {
    return 9;
  }

  // Show the status bar (it is guaranteed that this will only
  // be called during a V1-3 game).
  public void showStatusBar(String s,int a,int b,boolean flag)
  {
    String status;
    String s1, s2, s3;

    s1 = new String(" " + s + " ");
    if (flag) {
      s2 = new String(" Time: " + a + ":");
      if (b < 10)
      s2 += "0";
      s2 = s2 + b;
      s3 = new String(" ");
    }
    else {
      s2 = new String(" Score: " + a + " ");
      s3 = new String(" Turns: " + b + " ");
    }

    statusMessage = s1 + " " + s2 + s3;
  }

  // Split the screen, as per SPLIT_SCREEN
  public void splitScreen(int lines)
  {
  }

  // Set the current window, possibly clearing it.
  public void setCurrentWindow(int window)
  {
  }

  // Read a line of input from the current window.  If time is
  // nonzero, time out after time tenths of a second.  Return 0
  // if a timeout occurred, or the terminating character if it
  // did not.
  public int readLine(StringBuffer sb,int time)
  {
    if (commandQueue.isEmpty()) {
      quit();
    } else {
      currentCommand = nextCommand();
      sb.append(currentCommand.getCommandText());
    }
    return 10;
  }

  private ZaxCommand nextCommand() {
    if (currentCommand != null) {
      output.append(currentCommand.getOutput());
      currentCommand = null;
    }
    return commandQueue.remove();
  }

  // Read a single character from the current window
  public int readChar(int time)
  {
    StringBuffer sb = new StringBuffer();
    readLine(sb, 0);
    return (sb.length() > 0) ? sb.charAt(0) : 10;
  }

  // Display a string -- this method does a number of things, including scrolling only
  // as necessary, word-wrapping, and "more".
  public void showString(String s)
  {
    if (currentCommand != null) {
      currentCommand.addOutput(s);
    }
  }

  // Scroll the current window
  public void scrollWindow(int lines)
  {
  }

  // Erase a line in the current window
  public void eraseLine(int size)
  {
    fatal("eraseLine not yet implemented");
  }

  // Erase a window
  public void eraseWindow(int window)
  {
  }

  public InputStream getRestoreInputStream(String key)
  {
    return streamProvider.getInputStream(filenameForKey(key));
  }

  public void disposeInputStream(InputStream is) {
    streamProvider.disposeInputStream(is);
  }

  public OutputStream getSaveOutputStream(String key)
  {
    return streamProvider.getOutputStream(filenameForKey(key));
  }

  public void disposeOutputStream(OutputStream os) {
    streamProvider.disposeOutputStream(os);
  }

  private static String filenameForKey(String key) {
    if (key == null) {
      return "save.dat";
    } else {
      return "save-" + key + ".dat";
    }
  }

  // Set the current colors
  public void setColor(int fg,int bg)
  {
  }

  // Set the text style
  public void setTextStyle(int style)
  {
  }

  public void setFont(int font)
  {
  }

  public void setCursorPosition(int x, int y)
  {
  }

  // quit--end the program
  public void quit()
  {
    Thread curThread = cpuThread;
    cpuThread = null;
    curThread.stop();
  }

  // restart--prepare for a restart
  public void restart()
  {
    initialize(version);
  }
}

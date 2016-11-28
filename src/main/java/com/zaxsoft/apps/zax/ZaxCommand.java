package com.zaxsoft.apps.zax;

class ZaxCommand {
  private final String command;
  private final StringBuilder output;

  private ZaxCommand(final String command, final StringBuilder output) {
    this.command = command;
    this.output = output;
  }

  public static ZaxCommand recordingOutput(final String command) {
    return new ZaxCommand(command, new StringBuilder());
  }

  public static ZaxCommand ignoringOutput(final String command) {
    return new ZaxCommand(command, null);
  }

  public String getCommandText() {
    return command;
  }

  public void addOutput(final String output) {
    if (this.output != null && output != null) {
      String trimmed = output.trim();
      if (trimmed.endsWith(">")) {
        trimmed = trimmed.substring(0, trimmed.length()-1);
      }
      this.output.append(trimmed);
    }
  }

  public String getOutput() {
    return this.output == null ? "" : this.output.toString();
  }
}

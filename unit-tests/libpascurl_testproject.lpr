program libpascurl_testproject;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, timeintervaltestcase, datasizetestcase,
  pascurl, curlresult, timeinterval, datasize, errorstack, DNSProperty,
  httpstatuscode, errorstacktestcase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.


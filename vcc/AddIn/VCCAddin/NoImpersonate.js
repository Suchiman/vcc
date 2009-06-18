// NoImpersonate.js
// Performs a post-build fixup of an msi to change all deferred custom actions to NoImpersonate
// taken from http://blogs.gotdotnet.com/astebner/archive/2007/05/28/2958062.aspx

// Constant values from Windows Installer
var msiOpenDatabaseModeTransact = 1;

var msiViewModifyInsert = 1
var msiViewModifyUpdate = 2
var msiViewModifyAssign = 3
var msiViewModifyReplace = 4
var msiViewModifyDelete = 6

var msidbCustomActionTypeInScript = 0x00000400;
var msidbCustomActionTypeNoImpersonate = 0x00000800

if (WScript.Arguments.Length != 1)
{
  WScript.StdErr.WriteLine(WScript.ScriptName + " file");
  WScript.Quit(1);
}

var filespec = WScript.Arguments(0);
var installer = WScript.CreateObject("WindowsInstaller.Installer");
var database = installer.OpenDatabase(filespec, msiOpenDatabaseModeTransact);

var sql
var view
var record
/*  //PART TO INSERT A CustomActionDLL 
try
{
   // Insert the custom action to launch the application when finished
   sql = "INSERT INTO `CustomAction` (`Action`, `Type`, `Source`, `Target`) VALUES ('CustomActionDLL', '3073', '" + "MyBin" + "', '" + "SampleFunction" + "')";
   view = database.OpenView(sql);
   view.Execute(null);
   view.Close();
   
   // Insert Binary to Installer....
   var rec = installer.CreateRecord(1);
   var filename = "C:\\Users\\i-sebaf\\EMIC\\Verisoft\\Debug\\InstallHelper.dll";
   rec.SetStream(1, filename);
   
   sql = "INSERT INTO `Binary` (`Name`, `Data`) VALUES ('MyBin', ?)";
   view = database.OpenView(sql);
   view.Execute(rec);
   view.Close();

   // Execute Action before InstallValidate...
   sql = "INSERT INTO `InstallExecuteSequence` (`Action`, `Sequence`) VALUES ('CustomActionDLL', '1505')";
   view = database.OpenView(sql);
   view.Execute(null);
   view.Close();
}
catch(e)
{
  WScript.StdErr.WriteLine(e);
  WScript.Quit(1);
}
*/
try
{
  sql = "SELECT `Action`, `Type`, `Source`, `Target` FROM `CustomAction`";
  view = database.OpenView(sql);
  view.Execute();
  record = view.Fetch();
  while (record)
  {
    if (record.IntegerData(2) & msidbCustomActionTypeInScript)
    {
      record.IntegerData(2) = record.IntegerData(2) | msidbCustomActionTypeNoImpersonate;
      view.Modify(msiViewModifyReplace, record);
    }
    record = view.Fetch();
  }
  view.Close();
  database.Commit();
}
catch(e)
{
  WScript.StdErr.WriteLine(e);
  WScript.Quit(1);
}
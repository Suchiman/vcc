using System.ComponentModel;
using Microsoft.VisualStudio.Shell;

namespace Microsoft.Research.Vcc.VSPackage
{
    public class VccOptionPage : DialogPage
    {
        [Category("Language Version")]
        [DisplayName("VCC Language Version")]
        [Description("Choose the language version of VCC.")]
        public LanguageVersion LanguageVersion { get; set; }

        [Category("Additional Commandline Arguments")]
        [DisplayName("Custom Arguments")]
        [Description("These additional commandline arguments for VCC will be used every time VCC is executed.")]
        public string AdditionalCommandlineArguments { get; set; }

        [Category("Additional Commandline Arguments")]
        [DisplayName("Use Commandline Arguments")]
        [Description("Choose true to use the arguments you entered above, otherwise these arguments will be ignored.")]
        public bool UseAdditionalCommandlineArguments { get; set; }

        [DisplayName("Show Z3 Inspector")]
        [Description("Choose true to launch the Z3 Inspector to view the progress of verification.")]
        public bool ShowZ3Inspector{ get; set; }

        [DisplayName("Vcc Executable Folder")]
        [Editor(typeof(System.Windows.Forms.Design.FolderNameEditor),typeof(System.Drawing.Design.UITypeEditor))]
        [Description("The the folder in which your vcc.exe is located - this is usually" +
                      " not necessary. Leave this empty to use the path written to the registry while installing" +
                      " Vcc.")]
        public string VccExecutableFolder { get; set; }

        [DisplayName("Show Notifications")]
        [Description("Show notifications when a verfication run completes and Visual Studion is no longer the foreground window.")]
        public bool ShowNotifications { get; set; }

        [Category("Vcc Version")]
        [DisplayName("Installed Vcc Version")]
        [Description("The version of this extension and the vcc compiler.")]
        public string VccVersion { 
          get { return System.Diagnostics.FileVersionInfo.GetVersionInfo(typeof(VccOptionPage).Assembly.Location).FileVersion;; } 
        }
    }
}
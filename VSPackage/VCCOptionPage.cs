using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel;
using Microsoft.VisualStudio.Shell;
using System.IO;

namespace MicrosoftResearch.VSPackage
{

    public class VccOptionPage : DialogPage
    {
        [Category("Additional Commandline Arguments")]
        [DisplayName("Commandline Arguments")]
        [Description("Here you can place additional commandline arguments for VCC that will be used every time VCC is executed.")]
        public string AdditionalCommandlineArguments { get; set; }

        [Category("Additional Commandline Arguments")]
        [DisplayName("Use Commandline Arguments")]
        [Description("Choose true to use the arguments you entered above. If this is false, those arguments will be ignored.")]
        public bool UseAdditionalCommandlineArguments { get; set; }

        [DisplayName("Show Z3 Inspector")]
        [Description("Choose true to launch the Z3 Inspector to view the progress of verification.")]
        public bool ShowZ3Inspector{ get; set; }

        [DisplayName("Vcc executable Folder")]
        [Editor(typeof(System.Windows.Forms.Design.FolderNameEditor),typeof(System.Drawing.Design.UITypeEditor))]
        [Description("Here you can specify the folder in which your vcc.exe is located. This is usually" +
                      " not necessary. Leave this empty to use the path written to the registry while installing" +
                      " Vcc.")]
        public string VccExecutableFolder
        { get; set; }
    }
}
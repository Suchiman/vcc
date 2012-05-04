namespace Microsoft.Research.Vcc.VSPackage
{
    using System.Windows.Forms;

    public partial class CustomVerifyForm : Form
    {
        public CustomVerifyForm(string text)
        {
            InitializeComponent();
            this.textBox1.Text = text;
        }

        public string Arguments
        {
            get { return this.textBox1.Text; }
        }
    }
}

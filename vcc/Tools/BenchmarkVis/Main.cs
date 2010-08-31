using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace BenchmarkVis
{
  public partial class Main : Form
  {
    internal List<DataSet> data = new List<DataSet>();
    internal DataSet LeftDS, RightDS;
    internal Dictionary<string, int> testcaseId = new Dictionary<string, int>();
    internal List<string> testcaseNames = new List<string>();

    int MinScale;
    int MaxScale;
    int IdUnderMouse = -1;

    public Main()
    {
      InitializeComponent();

      this.SetStyle(ControlStyles.DoubleBuffer |
                    ControlStyles.UserPaint |
                    ControlStyles.AllPaintingInWmPaint,
                    true);
      this.UpdateStyles();

    }

    private void SetCommonRadio(RadioButton r, DataSet s)
    {
      r.AutoSize = true;
      r.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
      r.CheckedChanged += s.CheckedChanged;
    }

    private void AddDataSet(DataSet s)
    {
      data.Add(s);

      var y = data.Count() * 20;

      s.Left = new RadioButton();
      SetCommonRadio(s.Left, s);
      this.groupBox1.Controls.Add(s.Left);
      s.Left.Text = s.Name;
      s.Left.Location = new System.Drawing.Point(6, y);
      s.Left.CheckAlign = ContentAlignment.MiddleLeft;

      s.Right = new RadioButton();
      SetCommonRadio(s.Right, s);
      this.groupBox2.Controls.Add(s.Right);
      s.Right.Text = "vs.";
      s.Right.Location = new System.Drawing.Point(4, y);
      s.Right.CheckAlign = ContentAlignment.MiddleRight;
    }

    private int GetTestcaseId(string name)
    {
      var l = name.LastIndexOf('/');
      if(l>0) {
        name = name.Substring(l + 1);
      }
      int ret;
      if (testcaseId.TryGetValue(name, out ret))
        return ret;
      ret = testcaseId.Count;
      testcaseId.Add(name, ret);
      testcaseNames.Add(name);
      return ret;
    }

    internal void ProcessArgs()
    {
      var a = System.Environment.GetCommandLineArgs();
      for (int i = 1; i < a.Length; ++i)
        using (var rd = File.OpenText(a[i]))
          ReadData(rd);
      if (data.Count >= 2) {
        data[0].Left.Checked = true;
        data[1].Right.Checked = true;
      }
    }

    private void ReadData(TextReader rd)
    {
      DataSet d = new DataSet();
      d.parent = this;
      var seps = new char[] { ' ', '\t', '\n', '\r' };
      string sx_dir = "";
      string id = "";
      string options = "";
      int testId = -1;
      bool replaced = false;

      for (; ; ) {
        var l = rd.ReadLine();
        if (l == null) break;
        var words = l.Split(seps);
        if (words.Length < 2) continue;
        switch (words[0]) {
          case "SX_DIR": sx_dir = words[1]; break;
          case "ID":
            id = words[1];
            foreach (var ds in this.data) {
              if (ds.Id == id) {
                d = ds;
                replaced = true;
                break;
              }
            }
            break;
          case "OPTIONS":
            options = l.Replace("OPTIONS ", "").Replace("\n", "").Replace("\r", "");
            break;
          case "FILE":
            testId = GetTestcaseId(words[1]);
            break;
          case "TIME":
            var n = (int)(double.Parse(words[1]) * 1000);
            var dp = d.CreateDataPoint(testId);
            dp.Values.Add(n);
            break;
        }
      }

      if (!replaced) {
        d.Name = id.Replace("2010.","") + " " + sx_dir.Replace("sx-","") + " " + options.Replace("CASE_SPLIT","CS");
        d.LongName = id + " " + options + " " + sx_dir;
        d.Id = id;
        this.AddDataSet(d);
      }
    }

    private Point ScreenCoord(double x, double y)
    {
      if (x <= 0) x = 1;
      if (y <= 0) y = 1;
      double max = Math.Log10(MaxScale);
      return new Point((int)(Math.Log10(x)/max * panel1.Width),
                       (int)(panel1.Height - Math.Log10(y) / max * panel1.Height));
    }

    private void panel1_Paint(object sender, PaintEventArgs e)
    {
      var gfx = e.Graphics;
      var rect = gfx.ClipBounds;
      gfx.FillRectangle(Brushes.White, rect);

      if (LeftDS == null || RightDS == null) return;

      var len = Math.Min(LeftDS.Values.Count, RightDS.Values.Count);
      MinScale = int.MaxValue;
      MaxScale = int.MinValue;
      var sum1 = 0;
      var sum2 = 0;

      for (var i = 0; i < len; ++i) {
        var l = LeftDS.Values[i];
        var r = RightDS.Values[i];
        if (l != null && r != null) {
          MinScale = Math.Min(MinScale, Math.Min(l.Min, r.Min));
          MaxScale = Math.Max(MaxScale, Math.Max(l.Max, r.Max));
          sum1 += l.Avg;
          sum2 += r.Avg;
        }
      }

      if (MaxScale == int.MinValue) return;

      MaxScale += MaxScale / 3;

      Brush texts = new SolidBrush(Color.FromArgb(150, 150, 150));
      var font = groupBox1.Font;

      for (int scale = 10; scale < MaxScale; scale *= 10) {
        var p = ScreenCoord(scale, scale);
        gfx.DrawLine(Pens.LightGray, p.X, 0, p.X, panel1.Height);
        gfx.DrawLine(Pens.LightGray, 0, p.Y, panel1.Width, p.Y);
        var s = string.Format("{0:0.00}", scale / 1000.0);
        gfx.DrawString(s, font, texts, p.X, panel1.Height - 15);
        gfx.DrawString(s, font, texts, 3, p.Y + 2);
      }

      gfx.DrawLine(Pens.LightGray, 0, panel1.Height, panel1.Width, 0);
      var half = ScreenCoord(1, 2);
      Pen pn = new Pen(Color.FromArgb(230, 230, 230));
      gfx.DrawLine(pn, half.X, half.Y, half.X + panel1.Width, half.Y - panel1.Height);
      half = ScreenCoord(2, 1);
      gfx.DrawLine(pn, half.X, half.Y, half.X + panel1.Width, half.Y - panel1.Height);

      gfx.DrawString(string.Format("{0} ({1:0.00})",
        LeftDS.LongName, sum1 / 1000.0), font, texts, 2, 2);

      {
        var str = string.Format("{0} ({1:0.00})",
          RightDS.LongName, sum2 / 1000.0);
        var sz = gfx.MeasureString(str, font);
        gfx.DrawString(str, font, texts, panel1.Width - sz.Width, panel1.Height - 30);
      }

      for (var i = 0; i < len; ++i) {
        var l = LeftDS.Values[i];
        var r = RightDS.Values[i];
        if (l != null && r != null) {
          var selected = i == IdUnderMouse;
          var ll = ScreenCoord(r.Min, l.Max);
          var rr = ScreenCoord(r.Max, l.Min);
          var r2 = new Rectangle(ll.X, ll.Y, rr.X - ll.X, rr.Y - ll.Y);
          if (selected) {
            for (int j = 1; j < r.Values.Count - 1; ++j) {
              var v = r.Values[j];
              gfx.DrawLine(Pens.LightGray, ScreenCoord(v, l.Min), ScreenCoord(v, l.Max));
            }
            for (int j = 1; j < l.Values.Count - 1; ++j) {
              var v = l.Values[j];
              gfx.DrawLine(Pens.LightGray, ScreenCoord(r.Min,v), ScreenCoord(r.Max,v));
            }
          }
          gfx.DrawRectangle(selected ? Pens.Crimson : Pens.LightPink, r2);
          var mid = ScreenCoord(r.Avg, l.Avg);
          r2 = new Rectangle(mid.X - 1, mid.Y - 1, 3, 3);
          gfx.FillRectangle(selected ? Brushes.Red : Brushes.Blue, r2);
        }
      }

      foreach (var ds in data) {
        ds.Left.BackColor = Color.LightGray;
        ds.Right.BackColor = Color.LightGray;
      }

      if (sum1 < sum2) {
        //RightDS.Left.BackColor = Color.LightPink;
        LeftDS.Left.BackColor = Color.Lime;
      } else {
        //LeftDS.Left.BackColor = Color.LightPink;
        RightDS.Right.BackColor = Color.Lime;
      }

    }

    private void radioButton1_CheckedChanged(object sender, EventArgs e)
    {

    }

    internal void Flush()
    {
      panel1.Invalidate();
    }

    private void Main_Resize(object sender, EventArgs e)
    {
      Flush();
    }

    private void pictureBox1_MouseMove(object sender, MouseEventArgs e)
    {
      if (LeftDS == null || RightDS == null) return;

      var len = Math.Min(LeftDS.Values.Count, RightDS.Values.Count);
      var minDist = int.MaxValue;
      int id = -1;
      for (var i = 0; i < len; ++i) {
        var l = LeftDS.Values[i];
        var r = RightDS.Values[i];
        if (l != null && r != null) {
          var p = ScreenCoord(r.Avg, l.Avg);
          var dist = Math.Abs(p.X - e.X) + Math.Abs(p.Y - e.Y);
          if (dist < minDist) {
            minDist = dist;
            id = i;
          }
        }
      }

      if (minDist > 30)
        id = -1;
      if (IdUnderMouse != id) {
        IdUnderMouse = id;
        if (id >= 0) {
          toolStripStatusLabel1.Text = string.Format("{0} {1:0.00} {2:0.00}", testcaseNames[id], LeftDS.Values[id].Avg / 1000.0, RightDS.Values[id].Avg / 1000.0);
        } else {
          toolStripStatusLabel1.Text = "";
        }
        Flush();
      }
    }
  }

  class DataPoint
  {
    public List<int> Values = new List<int>();

    public int Min
    {
      get
      {
        var m = int.MaxValue;
        foreach (var v in Values) {
          if (v < m) m = v;
        }
        return m;
      }
    }

    public int Max
    {
      get
      {
        var m = int.MinValue;
        foreach (var v in Values) {
          if (v > m) m = v;
        }
        return m;
      }
    }

    public int Avg
    {
        get { return Median; }
    }

    public int Average
    {
      get
      {
        if(Values.Count == 0) return 0;
        var s = 0;
        foreach (var v in Values) {
          s += v;
        }
        return s / Values.Count;
      }
    }

    public int Median
    {
        get
        {
            if (Values.Count == 0) return 0;
            Values.Sort();
            var n =Values.Count;
            if (n % 2 == 0)
                return (Values[n / 2] + Values[n / 2 - 1]) / 2;
            else
                return Values[n / 2];
        }
    }
  }

  class DataSet
  {
    public RadioButton Left, Right;
    public string Name;
    public string Id;
    public string LongName;
    public Main parent;
    public List<DataPoint> Values = new List<DataPoint>();

    public DataPoint GetDataPoint(int id)
    {
      if (Values.Count > id)
        return Values[id];
      return null;
    }

    public DataPoint CreateDataPoint(int id)
    {
      var res = GetDataPoint(id);
      if (res != null) return res;
      while (Values.Count <= id)
        Values.Add(null);
      Values[id] = new DataPoint();
      return Values[id];
    }

    public void CheckedChanged(object sender, EventArgs e)
    {
      if (sender == Left) {
        if (Left.Checked) {
          parent.LeftDS = this;
          parent.Flush();
        }
      } else if (sender == Right) {
        if (Right.Checked) {
          parent.RightDS = this;
          parent.Flush();
        }
      }
    }
  }

}

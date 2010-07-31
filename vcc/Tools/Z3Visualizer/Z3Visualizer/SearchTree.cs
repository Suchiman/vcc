using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Z3AxiomProfiler.QuantifierModel;

namespace Z3AxiomProfiler
{
  public partial class SearchTree : Form
  {
    Model m;

    public SearchTree(Model m)
    {
      this.m = m;
      InitializeComponent();
      this.pictureBox1.Paint += this.PaintTree;
      this.MouseWheel += this.pictureBox1_MouseWheel;
    }

    Graphics gfx;
    float step;

#if false
    private void PaintSubtree(RectangleF r, Scope s)
    {
      int totalCnfl = 1;
      int maxInstanceDepth = 1;
      foreach (var c in s.ChildrenScopes) {
        totalCnfl += c.RecConflictCount;
        if (c.RecInstanceDepth > maxInstanceDepth)
          maxInstanceDepth = c.RecInstanceDepth;
      }
      int curCnfl = 0;

      //gfx.DrawRectangle(Pens.Red, r.X, r.Y, r.Width, r.Height);

      foreach (var c in s.ChildrenScopes) {
        var x = r.X + curCnfl * r.Width / totalCnfl;
        var w = c.RecConflictCount * r.Width / totalCnfl;
        curCnfl += c.RecConflictCount;

#if false
        var y = r.Y + (maxInstanceDepth - c.RecInstanceDepth) * r.Height / maxInstanceDepth;
        var h = (c.RecInstanceDepth - c.OwnInstanceCount) * r.Height / maxInstanceDepth;
#else
        var y = r.Y;
        var h = (c.RecInstanceDepth - c.OwnInstanceCount) * r.Height / c.RecInstanceDepth;
        if (c.RecInstanceDepth == 0)
          h = r.Height / 2;
#endif
        gfx.DrawLine(Pens.Black, r.X + r.Width / 2, r.Y + r.Height, x + w / 2, y + h);
        PaintSubtree(new RectangleF(x, y, w, h), c);
      }
    }
#endif

    float scale = 1.0f;
    float offX = 0, offY = 0;

    private PointF ToScreen(PointF p)
    {
      return new PointF(p.X * scale + offX, p.Y * scale + offY);
    }

    private void PaintSubtree(PointF ourPos, float ourAng, Scope s)
    {
      const float maxTotal = (float)(2 * Math.PI / 3);
      const float maxOne = (float)(Math.PI / 20);

      int n = s.ChildrenScopes.Count;
      if (n > 0) {
        float angStep = maxTotal / n;
        if (angStep > maxOne)
          angStep = maxOne;
        if (n == 1) angStep = 0;
        float curAng = ourAng - angStep * n / 2;
        foreach (var c in s.ChildrenScopes) {
          PointF t = ourPos;
          var len = c.OwnInstanceCount * step;
          t.X += (float)(Math.Sin(curAng) * len);
          t.Y -= (float)(Math.Cos(curAng) * len);
          gfx.DrawLine(Pens.Black, ToScreen(ourPos), ToScreen(t));
          PaintSubtree(t, curAng, c);
          curAng += angStep;
        }
      }
    }

    private void PaintTree(object sender, PaintEventArgs e)
    {
      var root = m.rootScope;
      while (root.ChildrenScopes.Count == 1)
        root = root.ChildrenScopes[0];

      gfx = e.Graphics;
      step = gfx.ClipBounds.Height /  (root.RecInstanceDepth - root.OwnInstanceCount);
      step *= 1.5f;
      var r = gfx.ClipBounds;
      gfx.FillRectangle(Brushes.White, r);
      //r = new RectangleF(0, 0, pictureBox1.Width, pictureBox1.Height);
      //gfx.Clip = new Region(r);
      PaintSubtree(new PointF(r.X + r.Width / 2, r.Bottom), 0, root);
    }

    private void pictureBox1_Resize(object sender, EventArgs e)
    {
      pictureBox1.Invalidate();
    }

    private void pictureBox1_Click(object sender, EventArgs e)
    {
    }

    private void pictureBox1_MouseClick(object sender, MouseEventArgs e)
    {
    }

    private void pictureBox1_MouseWheel(object sender, MouseEventArgs e)
    {
      if (e.Delta != 0) {
        var f = 1.2f;
        if (e.Delta < 0)
          f = 1 / f;

        scale *= f;

        offX = (1 - f) * pictureBox1.Width/2 + offX * f;
        offY = (1 - f) * pictureBox1.Height/2 + offY * f;
        pictureBox1.Invalidate();
      }
    }


    int prevX=-1, prevY;
    private void pictureBox1_MouseMove(object sender, MouseEventArgs e)
    {
      if (e.Button == System.Windows.Forms.MouseButtons.Left) {
        if (prevX != -1) {
          offX += e.X - prevX;
          offY += e.Y - prevY;
          pictureBox1.Invalidate();
        }
        prevX = e.X;
        prevY = e.Y;
      } else {
        prevX = -1;
      }
    }
  }
}

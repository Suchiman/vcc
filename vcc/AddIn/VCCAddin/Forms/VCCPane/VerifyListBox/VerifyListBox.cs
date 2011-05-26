//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

using System;
using System.Drawing;
using System.Windows.Forms;
using System.Windows.Forms.VisualStyles;

namespace VerifiedCCompilerAddin.Forms.UserControls {

  public class VerifyListBox : ListBox {

    ListBoxColors m_Colors = new ListBoxColors();

    public ListBoxColors Colors {
      get { return m_Colors; }
      set { m_Colors = value; }
    }

    public VerifyListBox() {
      this.DrawMode = DrawMode.OwnerDrawFixed;
      this.ItemHeight = 15;
    }

    protected override void OnMouseMove(MouseEventArgs e) {
      base.OnMouseMove(e);
      int index = this.IndexFromPoint(e.Location);
      if (index < 0)
        return;

      VerifyListItem CurrentItem = this.Items[index] as VerifyListItem;
      string TipText =  CurrentItem.Text;
    }

    protected override void OnDrawItem(DrawItemEventArgs e) {
      base.OnDrawItem(e);

      Graphics g = e.Graphics;
      
      //Positions
      int mLeft = e.Bounds.Left;
      int mTop = e.Bounds.Top;
      int mWidth = e.Bounds.Width;
      int mHeight = e.Bounds.Height;
      Point pt = new Point(mLeft, mTop);
      Rectangle rect = new Rectangle(15, mTop, mWidth - 15, mHeight);
          
      //Leeres Control zeichen...
      if (e.Index == -1) {
        e.DrawBackground();
        e.DrawFocusRectangle();
        return;
      }

      e.DrawBackground();

      if ((e.Index > -1) && (e.Index < this.Items.Count)) {
        //Nicht Type Safe, hier fehlt ein Check!!!
        VerifyListItem CurrentItem = this.Items[e.Index] as VerifyListItem;

        if (CurrentItem != null) {
                 
          //CheckBox
          CheckBoxState state = CheckBoxState.UncheckedNormal;
          if (CurrentItem.Checked) {
            state = CheckBoxState.CheckedNormal;
          }

          //Control Font
          Font ft = this.Font;
          Color c = m_Colors.Normal;
          
          switch (CurrentItem.State) {
		      case VerifyListItemState.Normal:
              c = m_Colors.Normal;
           break;
          case VerifyListItemState.Active:
               c = m_Colors.Active;
               ft = new Font(this.Font, FontStyle.Bold);
           break;
          case VerifyListItemState.Verified:
               c = m_Colors.Successful; 
           break;
          case VerifyListItemState.NotVerified:
               c = m_Colors.Failed;
           break;
          default:
           break;
          }
          
          //invertierte Darstellung bei Auswahl...
          if (CurrentItem.Selected) {
            byte R = (byte)(c.R ^ 255);
            byte G = (byte)(c.G ^ 255);
            byte B = (byte)(c.B ^ 255);
            c = Color.FromArgb(R, G, B);
          }

          Brush br = new SolidBrush(c);
          
          CheckBoxRenderer.DrawCheckBox(g, pt, state);
          
          if (CurrentItem.State == VerifyListItemState.Active) {
            int size = 103;                           //default size of ProgressBar.
            int pos = CurrentItem.Text.IndexOf('(');
            string showText = string.Empty;
            
            if (pos > 0) {
              showText = CurrentItem.Text.Substring(0, pos);
            } else {
              showText = CurrentItem.Text;
            }

            g.DrawString(showText, ft, br, new Rectangle(mLeft + 15, mTop, mWidth - 10 - size, mHeight));
            Rectangle rBar = new Rectangle(mLeft + mWidth - size, mTop+1, size-1, mHeight-2);
            ProgressBarRenderer.DrawHorizontalBar(g, rBar);
            Rectangle rChunks = new Rectangle(rBar.Left + 1, rBar.Top + 1, CurrentItem.Progess , rBar.Height - 2);
            ProgressBarRenderer.DrawHorizontalChunks(g, rChunks);
          }
          else {
            g.DrawString(CurrentItem.Text, ft, br, new PointF(mLeft + 15, mTop));
          }
        }
      }
    }

    protected override void OnMouseDown(MouseEventArgs e) {
      base.OnMouseUp(e);
      VerifyListItem CurrentItem = this.SelectedItem as VerifyListItem;
      if (CurrentItem == null)
        return;

      if (e.Button == MouseButtons.Left && CurrentItem.Selected) {   
        CurrentItem.Checked = !CurrentItem.Checked;
        this.Invalidate();
      }
    }

    protected override void OnKeyUp(KeyEventArgs e) {
      base.OnKeyUp(e);
      if (e.KeyCode == Keys.Space) {
        VerifyListItem CurrentItem = this.SelectedItem as VerifyListItem;
        CurrentItem.Checked = !CurrentItem.Checked;
        this.Invalidate();
      }
    }

    protected override void OnSelectedIndexChanged(EventArgs e) {
      base.OnSelectedIndexChanged(e);

      foreach (VerifyListItem Item in Items)
      {
        Item.Selected = false;
      }

      VerifyListItem SelectedItem = this.SelectedItem as VerifyListItem;
      if (SelectedItem == null)
        return;
      SelectedItem.Selected = true;

      this.Invalidate();
    }

 }

  public class ListBoxColors {
    public Color Active;
    public Color Failed;
    public Color Successful;
    public Color Normal;

    public ListBoxColors() {
      Active = Color.Orange;
      Failed = Color.Maroon;
      Successful = Color.DarkGreen;
      Normal = Color.Black;
    }
  }

}

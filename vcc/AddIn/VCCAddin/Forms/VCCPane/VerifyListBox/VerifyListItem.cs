//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

namespace VerifiedCCompilerAddin.Forms.UserControls {

  public class VerifyListItem {

    string m_sText = string.Empty;
    bool m_bChecked = false;
    int m_iProgress = 0;
    bool m_bSelected = false;
    VerifyListItemState m_State = VerifyListItemState.Normal;
   
    public VerifyListItem(string Text) {
      this.Text = Text;
      this.State = VerifyListItemState.Normal;
    }

    public override string ToString() {
      return Text;
    }

    #region Properties
    public bool Selected {
      get { return m_bSelected; }
      set { m_bSelected = value; }
    }
    public string Text {
      get {
        return m_sText;
      }

      set {
        m_sText = value;
      }
    }
    public bool Checked {
      get { return m_bChecked; }
      set { m_bChecked = value; }
    }
    public int Progess {
      get { return m_iProgress; }
      set { m_iProgress = value; }
    }
    public VerifyListItemState State {
      get { return m_State; }
      set { m_State = value; }
    }
    #endregion
  }

  /******************STATES************************/
  public enum VerifyListItemState {
    Normal,
    Active,
    Verified,
    NotVerified
  }
}

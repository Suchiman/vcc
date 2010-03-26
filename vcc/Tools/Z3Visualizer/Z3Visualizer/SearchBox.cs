using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Z3AxiomProfiler
{
  public partial class SearchBox : Form
  {
    public SearchBox(Z3AxiomProfiler a)
    {
      this.axprof = a;
      InitializeComponent();
    }

    class NodeText
    {
      internal TreeNode n;
      public NodeText(TreeNode n) { this.n = n; }
      public override string ToString()
      {
        return n.Text;
      }
    }

    List<NodeText> nodes = new List<NodeText>();
    Z3AxiomProfiler axprof;

    private void AddNodes(TreeNodeCollection coll)
    {
      foreach (TreeNode n in coll) {
        nodes.Add(new NodeText(n));
        if (n.IsExpanded) {
          AddNodes(n.Nodes);
        }
      }
    }

    public void SetFilter(string s)
    {
      var words0 = s.Split(' ');
      var words = new List<string>();
      foreach (var w in words0) if (w != "") words.Add(w.ToLower());
      var objs = new List<object>();
      foreach (var n in nodes) {
        bool wrong = false;
        string x = n.ToString().ToLower();
        foreach (var w in words) {
          if (!x.Contains(w)) { wrong = true; break; }
        }
        if (!wrong) objs.Add(n);
      }
      listBox1.BeginUpdate();
      listBox1.Items.Clear();
      listBox1.Items.AddRange(objs.ToArray());
      listBox1.EndUpdate();
    }

    public void Populate(TreeNodeCollection coll)
    {
      nodes.Clear();
      AddNodes(coll);
      SetFilter("");
    }

    private void textBox1_TextChanged(object sender, EventArgs e)
    {
      SetFilter(textBox1.Text);
    }

    private void SearchBox_FormClosing(object sender, FormClosingEventArgs e)
    {
    }

    private void textBox1_KeyPress(object sender, KeyPressEventArgs e)
    {
    }

    private void textBox1_KeyDown(object sender, KeyEventArgs e)
    {
      if (e.KeyCode == Keys.Down) {
        listBox1.SelectedIndex = 0;
        listBox1.Focus();
      }
    }

    private void Execute()
    {
      NodeText n = listBox1.SelectedItem as NodeText;
      if (n != null) {
        axprof.Activate(n.n);
        this.Hide();
      }
    }

    private void listBox1_Click(object sender, EventArgs e)
    {
      Execute();
    }

    private void listBox1_KeyDown(object sender, KeyEventArgs e)
    {
      if (e.KeyCode == Keys.Enter) {
        Execute();
        e.Handled = true;
      }
    }
  }
}

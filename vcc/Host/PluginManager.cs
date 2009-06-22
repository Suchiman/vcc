//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel.Composition;
using System.ComponentModel.Composition.Hosting;

namespace Microsoft.Research.Vcc
{
  class PluginManager
  {
    VccOptions options;

    [Import]
    public IEnumerable<Plugin> Plugins { get; set; }

    public PluginManager(VccOptions options)
    {
      this.options = options;
      List<string> directories;
      if (options.PluginOptions.TryGetValue("dir", out directories)) {
        foreach (var d in directories)
          AddPluginDirectory(d);
      }
    }

    AggregateCatalog directories = new AggregateCatalog();
    public void AddPluginDirectory(string dir)
    {
      directories.Catalogs.Add(new DirectoryCatalog(dir));
    }

    public void Discover()
    {
      this.Compose();
    }

    private void Compose()
    {
      var container = new CompositionContainer(directories);
      var batch = new CompositionBatch();
      batch.AddPart(this);
      container.Compose(batch);
    }
  }
}

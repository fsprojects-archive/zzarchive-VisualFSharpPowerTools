using System.Drawing;

namespace FSharpVSPowerTools
{
    partial class GeneralOptionsControl
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.grbOptions = new System.Windows.Forms.GroupBox();
            this.tblLayoutPanel = new System.Windows.Forms.TableLayoutPanel();
            this.flowLayoutPanel = new System.Windows.Forms.FlowLayoutPanel();
            this.chbXmlDoc = new System.Windows.Forms.CheckBox();
            this.chbFormatting = new System.Windows.Forms.CheckBox();
            this.chbNavBar = new System.Windows.Forms.CheckBox();
            this.chbHighlightUsage = new System.Windows.Forms.CheckBox();
            this.chbRenameRefactoring = new System.Windows.Forms.CheckBox();
            this.chbDepthColorizer = new System.Windows.Forms.CheckBox();
            this.chbNavigateTo = new System.Windows.Forms.CheckBox();
            this.tblLayoutPanelSyntax = new System.Windows.Forms.TableLayoutPanel();
            this.chbSyntaxColoring = new System.Windows.Forms.CheckBox();
            this.chbUnusedReferences = new System.Windows.Forms.CheckBox();
            this.chbUnusedOpens = new System.Windows.Forms.CheckBox();
            this.chbFolderOrganization = new System.Windows.Forms.CheckBox();
            this.chbFindAllReferences = new System.Windows.Forms.CheckBox();
            this.chbInterfaceImplementation = new System.Windows.Forms.CheckBox();
            this.chbRecordStubGeneration = new System.Windows.Forms.CheckBox();
            this.chbUnionPatternMatchCaseGeneration = new System.Windows.Forms.CheckBox();
            this.chbResolveUnopenedNamespaces = new System.Windows.Forms.CheckBox();
            this.chbGoToMetadata = new System.Windows.Forms.CheckBox();
            this.chbTaskListComments = new System.Windows.Forms.CheckBox();
            this.chbGenerateReferences = new System.Windows.Forms.CheckBox();
            this.chbGoToSymbolSource = new System.Windows.Forms.CheckBox();
            this.chbQuickInfoPanel = new System.Windows.Forms.CheckBox();
            this.lblInformation = new System.Windows.Forms.Label();
            this.lblInfo = new System.Windows.Forms.Label();
            this.chbLinter = new System.Windows.Forms.CheckBox();
            this.grbOptions.SuspendLayout();
            this.tblLayoutPanel.SuspendLayout();
            this.flowLayoutPanel.SuspendLayout();
            this.tblLayoutPanelSyntax.SuspendLayout();
            this.SuspendLayout();
            // 
            // grbOptions
            // 
            this.grbOptions.AutoSize = true;
            this.grbOptions.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.grbOptions.Controls.Add(this.tblLayoutPanel);
            this.grbOptions.Dock = System.Windows.Forms.DockStyle.Fill;
            this.grbOptions.Location = new System.Drawing.Point(0, 0);
            this.grbOptions.Margin = new System.Windows.Forms.Padding(0, 0, 3, 3);
            this.grbOptions.Name = "grbOptions";
            this.grbOptions.Padding = new System.Windows.Forms.Padding(1, 3, 1, 8);
            this.grbOptions.Size = new System.Drawing.Size(459, 324);
            this.grbOptions.TabIndex = 0;
            this.grbOptions.TabStop = false;
            this.grbOptions.Text = "Turn features on/off";
            // 
            // tblLayoutPanel
            // 
            this.tblLayoutPanel.AutoScroll = true;
            this.tblLayoutPanel.ColumnCount = 1;
            this.tblLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tblLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.Controls.Add(this.flowLayoutPanel, 0, 0);
            this.tblLayoutPanel.Controls.Add(this.lblInformation, 0, 1);
            this.tblLayoutPanel.Controls.Add(this.lblInfo, 0, 2);
            this.tblLayoutPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tblLayoutPanel.Location = new System.Drawing.Point(1, 16);
            this.tblLayoutPanel.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.tblLayoutPanel.Name = "tblLayoutPanel";
            this.tblLayoutPanel.RowCount = 3;
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanel.Size = new System.Drawing.Size(457, 300);
            this.tblLayoutPanel.TabIndex = 23;
            // 
            // flowLayoutPanel
            // 
            this.flowLayoutPanel.AutoScroll = true;
            this.flowLayoutPanel.Controls.Add(this.chbXmlDoc);
            this.flowLayoutPanel.Controls.Add(this.chbFormatting);
            this.flowLayoutPanel.Controls.Add(this.chbNavBar);
            this.flowLayoutPanel.Controls.Add(this.chbHighlightUsage);
            this.flowLayoutPanel.Controls.Add(this.chbRenameRefactoring);
            this.flowLayoutPanel.Controls.Add(this.chbDepthColorizer);
            this.flowLayoutPanel.Controls.Add(this.chbNavigateTo);
            this.flowLayoutPanel.Controls.Add(this.tblLayoutPanelSyntax);
            this.flowLayoutPanel.Controls.Add(this.chbFolderOrganization);
            this.flowLayoutPanel.Controls.Add(this.chbFindAllReferences);
            this.flowLayoutPanel.Controls.Add(this.chbInterfaceImplementation);
            this.flowLayoutPanel.Controls.Add(this.chbRecordStubGeneration);
            this.flowLayoutPanel.Controls.Add(this.chbUnionPatternMatchCaseGeneration);
            this.flowLayoutPanel.Controls.Add(this.chbResolveUnopenedNamespaces);
            this.flowLayoutPanel.Controls.Add(this.chbGoToMetadata);
            this.flowLayoutPanel.Controls.Add(this.chbTaskListComments);
            this.flowLayoutPanel.Controls.Add(this.chbGenerateReferences);
            this.flowLayoutPanel.Controls.Add(this.chbGoToSymbolSource);
            this.flowLayoutPanel.Controls.Add(this.chbQuickInfoPanel);
            this.flowLayoutPanel.Controls.Add(this.chbLinter);
            this.flowLayoutPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.flowLayoutPanel.FlowDirection = System.Windows.Forms.FlowDirection.TopDown;
            this.flowLayoutPanel.Location = new System.Drawing.Point(2, 2);
            this.flowLayoutPanel.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.flowLayoutPanel.Name = "flowLayoutPanel";
            this.flowLayoutPanel.Size = new System.Drawing.Size(453, 258);
            this.flowLayoutPanel.TabIndex = 0;
            // 
            // chbXmlDoc
            // 
            this.chbXmlDoc.AutoSize = true;
            this.chbXmlDoc.Checked = true;
            this.chbXmlDoc.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbXmlDoc.Location = new System.Drawing.Point(3, 3);
            this.chbXmlDoc.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbXmlDoc.Name = "chbXmlDoc";
            this.chbXmlDoc.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbXmlDoc.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.chbXmlDoc.Size = new System.Drawing.Size(151, 17);
            this.chbXmlDoc.TabIndex = 1;
            this.chbXmlDoc.Text = "Auto-generating XmlDoc";
            this.chbXmlDoc.UseVisualStyleBackColor = true;
            // 
            // chbFormatting
            // 
            this.chbFormatting.AutoSize = true;
            this.chbFormatting.Checked = true;
            this.chbFormatting.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbFormatting.Location = new System.Drawing.Point(3, 24);
            this.chbFormatting.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbFormatting.Name = "chbFormatting";
            this.chbFormatting.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbFormatting.Size = new System.Drawing.Size(146, 17);
            this.chbFormatting.TabIndex = 2;
            this.chbFormatting.Text = "Source code formatting";
            this.chbFormatting.UseVisualStyleBackColor = true;
            // 
            // chbNavBar
            // 
            this.chbNavBar.AutoSize = true;
            this.chbNavBar.Location = new System.Drawing.Point(3, 45);
            this.chbNavBar.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbNavBar.Name = "chbNavBar";
            this.chbNavBar.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbNavBar.Size = new System.Drawing.Size(120, 17);
            this.chbNavBar.TabIndex = 6;
            this.chbNavBar.Text = "Navigation bar ***";
            this.chbNavBar.UseVisualStyleBackColor = true;
            // 
            // chbHighlightUsage
            // 
            this.chbHighlightUsage.AutoSize = true;
            this.chbHighlightUsage.Checked = true;
            this.chbHighlightUsage.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbHighlightUsage.Location = new System.Drawing.Point(3, 66);
            this.chbHighlightUsage.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbHighlightUsage.Name = "chbHighlightUsage";
            this.chbHighlightUsage.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbHighlightUsage.Size = new System.Drawing.Size(130, 17);
            this.chbHighlightUsage.TabIndex = 7;
            this.chbHighlightUsage.Text = "Highlight references";
            this.chbHighlightUsage.UseVisualStyleBackColor = true;
            // 
            // chbRenameRefactoring
            // 
            this.chbRenameRefactoring.AutoSize = true;
            this.chbRenameRefactoring.Checked = true;
            this.chbRenameRefactoring.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbRenameRefactoring.Location = new System.Drawing.Point(3, 87);
            this.chbRenameRefactoring.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbRenameRefactoring.Name = "chbRenameRefactoring";
            this.chbRenameRefactoring.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbRenameRefactoring.Size = new System.Drawing.Size(129, 17);
            this.chbRenameRefactoring.TabIndex = 8;
            this.chbRenameRefactoring.Text = "Rename refactoring";
            this.chbRenameRefactoring.UseVisualStyleBackColor = true;
            // 
            // chbDepthColorizer
            // 
            this.chbDepthColorizer.AutoSize = true;
            this.chbDepthColorizer.Location = new System.Drawing.Point(3, 108);
            this.chbDepthColorizer.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbDepthColorizer.Name = "chbDepthColorizer";
            this.chbDepthColorizer.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbDepthColorizer.Size = new System.Drawing.Size(176, 17);
            this.chbDepthColorizer.TabIndex = 9;
            this.chbDepthColorizer.Text = "Indent guides/Depth colorizer";
            this.chbDepthColorizer.UseVisualStyleBackColor = true;
            // 
            // chbNavigateTo
            // 
            this.chbNavigateTo.AutoSize = true;
            this.chbNavigateTo.Checked = true;
            this.chbNavigateTo.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbNavigateTo.Location = new System.Drawing.Point(3, 129);
            this.chbNavigateTo.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbNavigateTo.Name = "chbNavigateTo";
            this.chbNavigateTo.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbNavigateTo.Size = new System.Drawing.Size(92, 17);
            this.chbNavigateTo.TabIndex = 10;
            this.chbNavigateTo.Text = "NavigateTo";
            this.chbNavigateTo.UseVisualStyleBackColor = true;
            // 
            // tblLayoutPanelSyntax
            // 
            this.tblLayoutPanelSyntax.AutoSize = true;
            this.tblLayoutPanelSyntax.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.tblLayoutPanelSyntax.ColumnCount = 1;
            this.tblLayoutPanelSyntax.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tblLayoutPanelSyntax.Controls.Add(this.chbSyntaxColoring, 0, 0);
            this.tblLayoutPanelSyntax.Controls.Add(this.chbUnusedReferences, 0, 2);
            this.tblLayoutPanelSyntax.Controls.Add(this.chbUnusedOpens, 0, 1);
            this.tblLayoutPanelSyntax.Location = new System.Drawing.Point(0, 147);
            this.tblLayoutPanelSyntax.Margin = new System.Windows.Forms.Padding(0);
            this.tblLayoutPanelSyntax.Name = "tblLayoutPanelSyntax";
            this.tblLayoutPanelSyntax.RowCount = 2;
            this.tblLayoutPanelSyntax.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 33.33333F));
            this.tblLayoutPanelSyntax.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 33.33333F));
            this.tblLayoutPanelSyntax.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 33.33333F));
            this.tblLayoutPanelSyntax.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 13F));
            this.tblLayoutPanelSyntax.Size = new System.Drawing.Size(204, 63);
            this.tblLayoutPanelSyntax.TabIndex = 11;
            // 
            // chbSyntaxColoring
            // 
            this.chbSyntaxColoring.AutoSize = true;
            this.chbSyntaxColoring.Checked = true;
            this.chbSyntaxColoring.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbSyntaxColoring.Location = new System.Drawing.Point(3, 3);
            this.chbSyntaxColoring.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbSyntaxColoring.Name = "chbSyntaxColoring";
            this.chbSyntaxColoring.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbSyntaxColoring.Size = new System.Drawing.Size(108, 17);
            this.chbSyntaxColoring.TabIndex = 11;
            this.chbSyntaxColoring.Text = "Syntax coloring";
            this.chbSyntaxColoring.UseVisualStyleBackColor = true;
            // 
            // chbUnusedReferences
            // 
            this.chbUnusedReferences.AutoSize = true;
            this.chbUnusedReferences.Checked = true;
            this.chbUnusedReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnusedReferences.Location = new System.Drawing.Point(27, 45);
            this.chbUnusedReferences.Margin = new System.Windows.Forms.Padding(27, 3, 3, 1);
            this.chbUnusedReferences.Name = "chbUnusedReferences";
            this.chbUnusedReferences.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbUnusedReferences.Size = new System.Drawing.Size(174, 17);
            this.chbUnusedReferences.TabIndex = 17;
            this.chbUnusedReferences.Text = "Gray out unused declarations";
            this.chbUnusedReferences.UseVisualStyleBackColor = true;
            // 
            // chbUnusedOpens
            // 
            this.chbUnusedOpens.AutoSize = true;
            this.chbUnusedOpens.Checked = true;
            this.chbUnusedOpens.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnusedOpens.Location = new System.Drawing.Point(27, 24);
            this.chbUnusedOpens.Margin = new System.Windows.Forms.Padding(27, 3, 3, 1);
            this.chbUnusedOpens.Name = "chbUnusedOpens";
            this.chbUnusedOpens.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbUnusedOpens.Size = new System.Drawing.Size(146, 17);
            this.chbUnusedOpens.TabIndex = 19;
            this.chbUnusedOpens.Text = "Gray out unused opens";
            this.chbUnusedOpens.UseVisualStyleBackColor = true;
            // 
            // chbFolderOrganization
            // 
            this.chbFolderOrganization.AutoSize = true;
            this.chbFolderOrganization.Location = new System.Drawing.Point(3, 213);
            this.chbFolderOrganization.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbFolderOrganization.Name = "chbFolderOrganization";
            this.chbFolderOrganization.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbFolderOrganization.Size = new System.Drawing.Size(125, 17);
            this.chbFolderOrganization.TabIndex = 12;
            this.chbFolderOrganization.Text = "Folder organization";
            this.chbFolderOrganization.UseVisualStyleBackColor = true;
            // 
            // chbFindAllReferences
            // 
            this.chbFindAllReferences.AutoSize = true;
            this.chbFindAllReferences.Checked = true;
            this.chbFindAllReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbFindAllReferences.Location = new System.Drawing.Point(3, 234);
            this.chbFindAllReferences.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbFindAllReferences.Name = "chbFindAllReferences";
            this.chbFindAllReferences.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbFindAllReferences.Size = new System.Drawing.Size(122, 17);
            this.chbFindAllReferences.TabIndex = 14;
            this.chbFindAllReferences.Text = "Find all references";
            this.chbFindAllReferences.UseVisualStyleBackColor = true;
            // 
            // chbInterfaceImplementation
            // 
            this.chbInterfaceImplementation.AutoSize = true;
            this.chbInterfaceImplementation.Checked = true;
            this.chbInterfaceImplementation.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbInterfaceImplementation.Location = new System.Drawing.Point(207, 3);
            this.chbInterfaceImplementation.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbInterfaceImplementation.Name = "chbInterfaceImplementation";
            this.chbInterfaceImplementation.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbInterfaceImplementation.Size = new System.Drawing.Size(128, 17);
            this.chbInterfaceImplementation.TabIndex = 12;
            this.chbInterfaceImplementation.Text = "Implement interface";
            this.chbInterfaceImplementation.UseVisualStyleBackColor = true;
            // 
            // chbRecordStubGeneration
            // 
            this.chbRecordStubGeneration.AutoSize = true;
            this.chbRecordStubGeneration.Checked = true;
            this.chbRecordStubGeneration.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbRecordStubGeneration.Location = new System.Drawing.Point(207, 24);
            this.chbRecordStubGeneration.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbRecordStubGeneration.Name = "chbRecordStubGeneration";
            this.chbRecordStubGeneration.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbRecordStubGeneration.Size = new System.Drawing.Size(147, 17);
            this.chbRecordStubGeneration.TabIndex = 15;
            this.chbRecordStubGeneration.Text = "Record stub generation";
            this.chbRecordStubGeneration.UseVisualStyleBackColor = true;
            // 
            // chbUnionPatternMatchCaseGeneration
            // 
            this.chbUnionPatternMatchCaseGeneration.AutoSize = true;
            this.chbUnionPatternMatchCaseGeneration.Checked = true;
            this.chbUnionPatternMatchCaseGeneration.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnionPatternMatchCaseGeneration.Location = new System.Drawing.Point(207, 45);
            this.chbUnionPatternMatchCaseGeneration.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbUnionPatternMatchCaseGeneration.Name = "chbUnionPatternMatchCaseGeneration";
            this.chbUnionPatternMatchCaseGeneration.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbUnionPatternMatchCaseGeneration.Size = new System.Drawing.Size(211, 17);
            this.chbUnionPatternMatchCaseGeneration.TabIndex = 16;
            this.chbUnionPatternMatchCaseGeneration.Text = "Union pattern match case generation";
            this.chbUnionPatternMatchCaseGeneration.UseVisualStyleBackColor = true;
            // 
            // chbResolveUnopenedNamespaces
            // 
            this.chbResolveUnopenedNamespaces.AutoSize = true;
            this.chbResolveUnopenedNamespaces.Checked = true;
            this.chbResolveUnopenedNamespaces.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbResolveUnopenedNamespaces.Location = new System.Drawing.Point(207, 66);
            this.chbResolveUnopenedNamespaces.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbResolveUnopenedNamespaces.Name = "chbResolveUnopenedNamespaces";
            this.chbResolveUnopenedNamespaces.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbResolveUnopenedNamespaces.Size = new System.Drawing.Size(189, 17);
            this.chbResolveUnopenedNamespaces.TabIndex = 16;
            this.chbResolveUnopenedNamespaces.Text = "Resolve unopened namespaces";
            this.chbResolveUnopenedNamespaces.UseVisualStyleBackColor = true;
            // 
            // chbGoToMetadata
            // 
            this.chbGoToMetadata.AutoSize = true;
            this.chbGoToMetadata.Checked = true;
            this.chbGoToMetadata.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGoToMetadata.Location = new System.Drawing.Point(207, 87);
            this.chbGoToMetadata.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbGoToMetadata.Name = "chbGoToMetadata";
            this.chbGoToMetadata.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbGoToMetadata.Size = new System.Drawing.Size(109, 17);
            this.chbGoToMetadata.TabIndex = 18;
            this.chbGoToMetadata.Text = "Go to metadata";
            this.chbGoToMetadata.UseVisualStyleBackColor = true;
            // 
            // chbTaskListComments
            // 
            this.chbTaskListComments.AutoSize = true;
            this.chbTaskListComments.Checked = true;
            this.chbTaskListComments.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbTaskListComments.Location = new System.Drawing.Point(207, 108);
            this.chbTaskListComments.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbTaskListComments.Name = "chbTaskListComments";
            this.chbTaskListComments.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbTaskListComments.Size = new System.Drawing.Size(130, 17);
            this.chbTaskListComments.TabIndex = 18;
            this.chbTaskListComments.Text = "Task List comments";
            this.chbTaskListComments.UseVisualStyleBackColor = true;
            // 
            // chbGenerateReferences
            // 
            this.chbGenerateReferences.AutoSize = true;
            this.chbGenerateReferences.Checked = true;
            this.chbGenerateReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGenerateReferences.Location = new System.Drawing.Point(207, 129);
            this.chbGenerateReferences.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbGenerateReferences.Name = "chbGenerateReferences";
            this.chbGenerateReferences.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbGenerateReferences.Size = new System.Drawing.Size(167, 17);
            this.chbGenerateReferences.TabIndex = 20;
            this.chbGenerateReferences.Text = "Generate references for FSI";
            this.chbGenerateReferences.UseVisualStyleBackColor = true;
            // 
            // chbGoToSymbolSource
            // 
            this.chbGoToSymbolSource.AutoSize = true;
            this.chbGoToSymbolSource.Checked = true;
            this.chbGoToSymbolSource.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGoToSymbolSource.Location = new System.Drawing.Point(207, 150);
            this.chbGoToSymbolSource.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbGoToSymbolSource.Name = "chbGoToSymbolSource";
            this.chbGoToSymbolSource.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbGoToSymbolSource.Size = new System.Drawing.Size(126, 17);
            this.chbGoToSymbolSource.TabIndex = 21;
            this.chbGoToSymbolSource.Text = "Navigate to source";
            this.chbGoToSymbolSource.UseVisualStyleBackColor = true;
            // 
            // chbQuickInfoPanel
            // 
            this.chbQuickInfoPanel.AutoSize = true;
            this.chbQuickInfoPanel.Checked = true;
            this.chbQuickInfoPanel.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbQuickInfoPanel.Location = new System.Drawing.Point(207, 171);
            this.chbQuickInfoPanel.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbQuickInfoPanel.Name = "chbQuickInfoPanel";
            this.chbQuickInfoPanel.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbQuickInfoPanel.Size = new System.Drawing.Size(113, 17);
            this.chbQuickInfoPanel.TabIndex = 22;
            this.chbQuickInfoPanel.Text = "Quick info panel";
            this.chbQuickInfoPanel.UseVisualStyleBackColor = true;
            // 
            // lblInformation
            // 
            this.lblInformation.AutoSize = true;
            this.lblInformation.Location = new System.Drawing.Point(3, 262);
            this.lblInformation.Name = "lblInformation";
            this.lblInformation.Padding = new System.Windows.Forms.Padding(5, 3, 5, 3);
            this.lblInformation.Size = new System.Drawing.Size(149, 19);
            this.lblInformation.TabIndex = 13;
            this.lblInformation.Text = "*** Admin privileges required";
            // 
            // lblInfo
            // 
            this.lblInfo.AutoSize = true;
            this.lblInfo.ForeColor = System.Drawing.SystemColors.HotTrack;
            this.lblInfo.Location = new System.Drawing.Point(3, 281);
            this.lblInfo.Name = "lblInfo";
            this.lblInfo.Padding = new System.Windows.Forms.Padding(5, 3, 5, 3);
            this.lblInfo.Size = new System.Drawing.Size(341, 19);
            this.lblInfo.TabIndex = 5;
            this.lblInfo.Text = "You must restart Visual Studio in order for the changes to take effect.";
            // 
            // chbLinter
            // 
            this.chbLinter.AutoSize = true;
            this.chbLinter.Checked = true;
            this.chbLinter.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbLinter.Location = new System.Drawing.Point(207, 192);
            this.chbLinter.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.chbLinter.Name = "chbLinter";
            this.chbLinter.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.chbLinter.Size = new System.Drawing.Size(62, 17);
            this.chbLinter.TabIndex = 23;
            this.chbLinter.Text = "Linter";
            this.chbLinter.UseVisualStyleBackColor = true;
            // 
            // GeneralOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grbOptions);
            this.Name = "GeneralOptionsControl";
            this.Size = new System.Drawing.Size(459, 324);
            this.Load += new System.EventHandler(this.GeneralOptionsControl_Load);
            this.grbOptions.ResumeLayout(false);
            this.tblLayoutPanel.ResumeLayout(false);
            this.tblLayoutPanel.PerformLayout();
            this.flowLayoutPanel.ResumeLayout(false);
            this.flowLayoutPanel.PerformLayout();
            this.tblLayoutPanelSyntax.ResumeLayout(false);
            this.tblLayoutPanelSyntax.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.GroupBox grbOptions;
        private System.Windows.Forms.CheckBox chbFormatting;
        private System.Windows.Forms.CheckBox chbXmlDoc;
        private System.Windows.Forms.CheckBox chbNavBar;
        private System.Windows.Forms.CheckBox chbHighlightUsage;
        private System.Windows.Forms.CheckBox chbRenameRefactoring;
        private System.Windows.Forms.CheckBox chbDepthColorizer;
        private System.Windows.Forms.CheckBox chbNavigateTo;
        private System.Windows.Forms.CheckBox chbFolderOrganization;
        private System.Windows.Forms.CheckBox chbSyntaxColoring;
        private System.Windows.Forms.CheckBox chbInterfaceImplementation;
        private System.Windows.Forms.Label lblInformation;
        private System.Windows.Forms.CheckBox chbFindAllReferences;
        private System.Windows.Forms.CheckBox chbRecordStubGeneration;
        private System.Windows.Forms.CheckBox chbUnionPatternMatchCaseGeneration;
        private System.Windows.Forms.CheckBox chbResolveUnopenedNamespaces;
        private System.Windows.Forms.CheckBox chbUnusedReferences;
        private System.Windows.Forms.CheckBox chbTaskListComments;
        private System.Windows.Forms.CheckBox chbGoToMetadata;
        private System.Windows.Forms.CheckBox chbUnusedOpens;
        private System.Windows.Forms.CheckBox chbGenerateReferences;
        private System.Windows.Forms.CheckBox chbGoToSymbolSource;
        private System.Windows.Forms.CheckBox chbQuickInfoPanel;
        private System.Windows.Forms.TableLayoutPanel tblLayoutPanel;
        private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel;
        private System.Windows.Forms.TableLayoutPanel tblLayoutPanelSyntax;
        private System.Windows.Forms.Label lblInfo;
        private System.Windows.Forms.CheckBox chbLinter;
    }
}

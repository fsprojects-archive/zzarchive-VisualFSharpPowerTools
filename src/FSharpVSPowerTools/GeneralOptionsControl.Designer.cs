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
            this.grbOptions.Margin = new System.Windows.Forms.Padding(0, 0, 4, 5);
            this.grbOptions.Name = "grbOptions";
            this.grbOptions.Padding = new System.Windows.Forms.Padding(2, 4, 2, 12);
            this.grbOptions.Size = new System.Drawing.Size(689, 498);
            this.grbOptions.TabIndex = 0;
            this.grbOptions.TabStop = false;
            this.grbOptions.Text = "Turn features on/off";
            // 
            // tblLayoutPanel
            // 
            this.tblLayoutPanel.AutoScroll = true;
            this.tblLayoutPanel.ColumnCount = 1;
            this.tblLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tblLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.Controls.Add(this.flowLayoutPanel, 0, 0);
            this.tblLayoutPanel.Controls.Add(this.lblInformation, 0, 1);
            this.tblLayoutPanel.Controls.Add(this.lblInfo, 0, 2);
            this.tblLayoutPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tblLayoutPanel.Location = new System.Drawing.Point(2, 23);
            this.tblLayoutPanel.Name = "tblLayoutPanel";
            this.tblLayoutPanel.RowCount = 3;
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanel.Size = new System.Drawing.Size(685, 463);
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
            this.flowLayoutPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.flowLayoutPanel.FlowDirection = System.Windows.Forms.FlowDirection.TopDown;
            this.flowLayoutPanel.Location = new System.Drawing.Point(3, 3);
            this.flowLayoutPanel.Name = "flowLayoutPanel";
            this.flowLayoutPanel.Size = new System.Drawing.Size(679, 401);
            this.flowLayoutPanel.TabIndex = 0;
            // 
            // chbXmlDoc
            // 
            this.chbXmlDoc.AutoSize = true;
            this.chbXmlDoc.Checked = true;
            this.chbXmlDoc.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbXmlDoc.Location = new System.Drawing.Point(4, 5);
            this.chbXmlDoc.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbXmlDoc.Name = "chbXmlDoc";
            this.chbXmlDoc.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbXmlDoc.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.chbXmlDoc.Size = new System.Drawing.Size(226, 24);
            this.chbXmlDoc.TabIndex = 1;
            this.chbXmlDoc.Text = "Auto-generating XmlDoc";
            this.chbXmlDoc.UseVisualStyleBackColor = true;
            // 
            // chbFormatting
            // 
            this.chbFormatting.AutoSize = true;
            this.chbFormatting.Checked = true;
            this.chbFormatting.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbFormatting.Location = new System.Drawing.Point(4, 36);
            this.chbFormatting.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbFormatting.Name = "chbFormatting";
            this.chbFormatting.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbFormatting.Size = new System.Drawing.Size(217, 24);
            this.chbFormatting.TabIndex = 2;
            this.chbFormatting.Text = "Source code formatting";
            this.chbFormatting.UseVisualStyleBackColor = true;
            // 
            // chbNavBar
            // 
            this.chbNavBar.AutoSize = true;
            this.chbNavBar.Location = new System.Drawing.Point(4, 67);
            this.chbNavBar.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbNavBar.Name = "chbNavBar";
            this.chbNavBar.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbNavBar.Size = new System.Drawing.Size(174, 24);
            this.chbNavBar.TabIndex = 6;
            this.chbNavBar.Text = "Navigation bar ***";
            this.chbNavBar.UseVisualStyleBackColor = true;
            // 
            // chbHighlightUsage
            // 
            this.chbHighlightUsage.AutoSize = true;
            this.chbHighlightUsage.Checked = true;
            this.chbHighlightUsage.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbHighlightUsage.Location = new System.Drawing.Point(4, 98);
            this.chbHighlightUsage.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbHighlightUsage.Name = "chbHighlightUsage";
            this.chbHighlightUsage.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbHighlightUsage.Size = new System.Drawing.Size(193, 24);
            this.chbHighlightUsage.TabIndex = 7;
            this.chbHighlightUsage.Text = "Highlight references";
            this.chbHighlightUsage.UseVisualStyleBackColor = true;
            // 
            // chbRenameRefactoring
            // 
            this.chbRenameRefactoring.AutoSize = true;
            this.chbRenameRefactoring.Checked = true;
            this.chbRenameRefactoring.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbRenameRefactoring.Location = new System.Drawing.Point(4, 129);
            this.chbRenameRefactoring.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbRenameRefactoring.Name = "chbRenameRefactoring";
            this.chbRenameRefactoring.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbRenameRefactoring.Size = new System.Drawing.Size(192, 24);
            this.chbRenameRefactoring.TabIndex = 8;
            this.chbRenameRefactoring.Text = "Rename refactoring";
            this.chbRenameRefactoring.UseVisualStyleBackColor = true;
            // 
            // chbDepthColorizer
            // 
            this.chbDepthColorizer.AutoSize = true;
            this.chbDepthColorizer.Location = new System.Drawing.Point(4, 160);
            this.chbDepthColorizer.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbDepthColorizer.Name = "chbDepthColorizer";
            this.chbDepthColorizer.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbDepthColorizer.Size = new System.Drawing.Size(259, 24);
            this.chbDepthColorizer.TabIndex = 9;
            this.chbDepthColorizer.Text = "Indent guides/Depth colorizer";
            this.chbDepthColorizer.UseVisualStyleBackColor = true;
            // 
            // chbNavigateTo
            // 
            this.chbNavigateTo.AutoSize = true;
            this.chbNavigateTo.Checked = true;
            this.chbNavigateTo.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbNavigateTo.Location = new System.Drawing.Point(4, 191);
            this.chbNavigateTo.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbNavigateTo.Name = "chbNavigateTo";
            this.chbNavigateTo.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbNavigateTo.Size = new System.Drawing.Size(131, 24);
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
            this.tblLayoutPanelSyntax.Location = new System.Drawing.Point(0, 217);
            this.tblLayoutPanelSyntax.Margin = new System.Windows.Forms.Padding(0);
            this.tblLayoutPanelSyntax.Name = "tblLayoutPanelSyntax";
            this.tblLayoutPanelSyntax.RowCount = 2;
            this.tblLayoutPanelSyntax.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 33.33333F));
            this.tblLayoutPanelSyntax.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 33.33333F));
            this.tblLayoutPanelSyntax.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 33.33333F));
            this.tblLayoutPanelSyntax.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20F));
            this.tblLayoutPanelSyntax.Size = new System.Drawing.Size(303, 93);
            this.tblLayoutPanelSyntax.TabIndex = 11;
            // 
            // chbSyntaxColoring
            // 
            this.chbSyntaxColoring.AutoSize = true;
            this.chbSyntaxColoring.Checked = true;
            this.chbSyntaxColoring.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbSyntaxColoring.Location = new System.Drawing.Point(4, 5);
            this.chbSyntaxColoring.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbSyntaxColoring.Name = "chbSyntaxColoring";
            this.chbSyntaxColoring.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbSyntaxColoring.Size = new System.Drawing.Size(158, 24);
            this.chbSyntaxColoring.TabIndex = 11;
            this.chbSyntaxColoring.Text = "Syntax coloring";
            this.chbSyntaxColoring.UseVisualStyleBackColor = true;
            // 
            // chbUnusedReferences
            // 
            this.chbUnusedReferences.AutoSize = true;
            this.chbUnusedReferences.Checked = true;
            this.chbUnusedReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnusedReferences.Location = new System.Drawing.Point(40, 67);
            this.chbUnusedReferences.Margin = new System.Windows.Forms.Padding(40, 5, 4, 2);
            this.chbUnusedReferences.Name = "chbUnusedReferences";
            this.chbUnusedReferences.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbUnusedReferences.Size = new System.Drawing.Size(259, 24);
            this.chbUnusedReferences.TabIndex = 17;
            this.chbUnusedReferences.Text = "Gray out unused declarations";
            this.chbUnusedReferences.UseVisualStyleBackColor = true;
            // 
            // chbUnusedOpens
            // 
            this.chbUnusedOpens.AutoSize = true;
            this.chbUnusedOpens.Checked = true;
            this.chbUnusedOpens.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnusedOpens.Location = new System.Drawing.Point(40, 36);
            this.chbUnusedOpens.Margin = new System.Windows.Forms.Padding(40, 5, 4, 2);
            this.chbUnusedOpens.Name = "chbUnusedOpens";
            this.chbUnusedOpens.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbUnusedOpens.Size = new System.Drawing.Size(217, 24);
            this.chbUnusedOpens.TabIndex = 19;
            this.chbUnusedOpens.Text = "Gray out unused opens";
            this.chbUnusedOpens.UseVisualStyleBackColor = true;
            // 
            // chbFolderOrganization
            // 
            this.chbFolderOrganization.AutoSize = true;
            this.chbFolderOrganization.Location = new System.Drawing.Point(4, 315);
            this.chbFolderOrganization.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbFolderOrganization.Name = "chbFolderOrganization";
            this.chbFolderOrganization.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbFolderOrganization.Size = new System.Drawing.Size(187, 24);
            this.chbFolderOrganization.TabIndex = 12;
            this.chbFolderOrganization.Text = "Folder organization";
            this.chbFolderOrganization.UseVisualStyleBackColor = true;
            // 
            // chbFindAllReferences
            // 
            this.chbFindAllReferences.AutoSize = true;
            this.chbFindAllReferences.Checked = true;
            this.chbFindAllReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbFindAllReferences.Location = new System.Drawing.Point(4, 346);
            this.chbFindAllReferences.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbFindAllReferences.Name = "chbFindAllReferences";
            this.chbFindAllReferences.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbFindAllReferences.Size = new System.Drawing.Size(181, 24);
            this.chbFindAllReferences.TabIndex = 14;
            this.chbFindAllReferences.Text = "Find all references";
            this.chbFindAllReferences.UseVisualStyleBackColor = true;
            // 
            // chbInterfaceImplementation
            // 
            this.chbInterfaceImplementation.AutoSize = true;
            this.chbInterfaceImplementation.Checked = true;
            this.chbInterfaceImplementation.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbInterfaceImplementation.Location = new System.Drawing.Point(307, 5);
            this.chbInterfaceImplementation.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbInterfaceImplementation.Name = "chbInterfaceImplementation";
            this.chbInterfaceImplementation.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbInterfaceImplementation.Size = new System.Drawing.Size(192, 24);
            this.chbInterfaceImplementation.TabIndex = 12;
            this.chbInterfaceImplementation.Text = "Implement interface";
            this.chbInterfaceImplementation.UseVisualStyleBackColor = true;
            // 
            // chbRecordStubGeneration
            // 
            this.chbRecordStubGeneration.AutoSize = true;
            this.chbRecordStubGeneration.Checked = true;
            this.chbRecordStubGeneration.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbRecordStubGeneration.Location = new System.Drawing.Point(307, 36);
            this.chbRecordStubGeneration.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbRecordStubGeneration.Name = "chbRecordStubGeneration";
            this.chbRecordStubGeneration.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbRecordStubGeneration.Size = new System.Drawing.Size(218, 24);
            this.chbRecordStubGeneration.TabIndex = 15;
            this.chbRecordStubGeneration.Text = "Record stub generation";
            this.chbRecordStubGeneration.UseVisualStyleBackColor = true;
            // 
            // chbUnionPatternMatchCaseGeneration
            // 
            this.chbUnionPatternMatchCaseGeneration.AutoSize = true;
            this.chbUnionPatternMatchCaseGeneration.Checked = true;
            this.chbUnionPatternMatchCaseGeneration.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnionPatternMatchCaseGeneration.Location = new System.Drawing.Point(307, 67);
            this.chbUnionPatternMatchCaseGeneration.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbUnionPatternMatchCaseGeneration.Name = "chbUnionPatternMatchCaseGeneration";
            this.chbUnionPatternMatchCaseGeneration.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbUnionPatternMatchCaseGeneration.Size = new System.Drawing.Size(314, 24);
            this.chbUnionPatternMatchCaseGeneration.TabIndex = 16;
            this.chbUnionPatternMatchCaseGeneration.Text = "Union pattern match case generation";
            this.chbUnionPatternMatchCaseGeneration.UseVisualStyleBackColor = true;
            // 
            // chbResolveUnopenedNamespaces
            // 
            this.chbResolveUnopenedNamespaces.AutoSize = true;
            this.chbResolveUnopenedNamespaces.Checked = true;
            this.chbResolveUnopenedNamespaces.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbResolveUnopenedNamespaces.Location = new System.Drawing.Point(307, 98);
            this.chbResolveUnopenedNamespaces.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbResolveUnopenedNamespaces.Name = "chbResolveUnopenedNamespaces";
            this.chbResolveUnopenedNamespaces.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbResolveUnopenedNamespaces.Size = new System.Drawing.Size(279, 24);
            this.chbResolveUnopenedNamespaces.TabIndex = 16;
            this.chbResolveUnopenedNamespaces.Text = "Resolve unopened namespaces";
            this.chbResolveUnopenedNamespaces.UseVisualStyleBackColor = true;
            // 
            // chbGoToMetadata
            // 
            this.chbGoToMetadata.AutoSize = true;
            this.chbGoToMetadata.Checked = true;
            this.chbGoToMetadata.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGoToMetadata.Location = new System.Drawing.Point(307, 129);
            this.chbGoToMetadata.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbGoToMetadata.Name = "chbGoToMetadata";
            this.chbGoToMetadata.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbGoToMetadata.Size = new System.Drawing.Size(163, 24);
            this.chbGoToMetadata.TabIndex = 18;
            this.chbGoToMetadata.Text = "Go to metadata";
            this.chbGoToMetadata.UseVisualStyleBackColor = true;
            // 
            // chbTaskListComments
            // 
            this.chbTaskListComments.AutoSize = true;
            this.chbTaskListComments.Checked = true;
            this.chbTaskListComments.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbTaskListComments.Location = new System.Drawing.Point(307, 160);
            this.chbTaskListComments.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbTaskListComments.Name = "chbTaskListComments";
            this.chbTaskListComments.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbTaskListComments.Size = new System.Drawing.Size(192, 24);
            this.chbTaskListComments.TabIndex = 18;
            this.chbTaskListComments.Text = "Task List comments";
            this.chbTaskListComments.UseVisualStyleBackColor = true;
            // 
            // chbGenerateReferences
            // 
            this.chbGenerateReferences.AutoSize = true;
            this.chbGenerateReferences.Checked = true;
            this.chbGenerateReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGenerateReferences.Location = new System.Drawing.Point(307, 191);
            this.chbGenerateReferences.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbGenerateReferences.Name = "chbGenerateReferences";
            this.chbGenerateReferences.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbGenerateReferences.Size = new System.Drawing.Size(252, 24);
            this.chbGenerateReferences.TabIndex = 20;
            this.chbGenerateReferences.Text = "Generate references for FSI";
            this.chbGenerateReferences.UseVisualStyleBackColor = true;
            // 
            // chbGoToSymbolSource
            // 
            this.chbGoToSymbolSource.AutoSize = true;
            this.chbGoToSymbolSource.Checked = true;
            this.chbGoToSymbolSource.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGoToSymbolSource.Location = new System.Drawing.Point(307, 222);
            this.chbGoToSymbolSource.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbGoToSymbolSource.Name = "chbGoToSymbolSource";
            this.chbGoToSymbolSource.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbGoToSymbolSource.Size = new System.Drawing.Size(183, 24);
            this.chbGoToSymbolSource.TabIndex = 21;
            this.chbGoToSymbolSource.Text = "Navigate to source";
            this.chbGoToSymbolSource.UseVisualStyleBackColor = true;
            // 
            // chbQuickInfoPanel
            // 
            this.chbQuickInfoPanel.AutoSize = true;
            this.chbQuickInfoPanel.Checked = true;
            this.chbQuickInfoPanel.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbQuickInfoPanel.Location = new System.Drawing.Point(307, 253);
            this.chbQuickInfoPanel.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.chbQuickInfoPanel.Name = "chbQuickInfoPanel";
            this.chbQuickInfoPanel.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.chbQuickInfoPanel.Size = new System.Drawing.Size(164, 24);
            this.chbQuickInfoPanel.TabIndex = 22;
            this.chbQuickInfoPanel.Text = "Quick info panel";
            this.chbQuickInfoPanel.UseVisualStyleBackColor = true;
            // 
            // lblInformation
            // 
            this.lblInformation.AutoSize = true;
            this.lblInformation.Location = new System.Drawing.Point(4, 407);
            this.lblInformation.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblInformation.Name = "lblInformation";
            this.lblInformation.Padding = new System.Windows.Forms.Padding(8, 4, 8, 4);
            this.lblInformation.Size = new System.Drawing.Size(347, 28);
            this.lblInformation.TabIndex = 13;
            this.lblInformation.Text = "*** Admin privileges required to enable/disable";
            // 
            // lblInfo
            // 
            this.lblInfo.AutoSize = true;
            this.lblInfo.ForeColor = System.Drawing.SystemColors.HotTrack;
            this.lblInfo.Location = new System.Drawing.Point(4, 435);
            this.lblInfo.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblInfo.Name = "lblInfo";
            this.lblInfo.Padding = new System.Windows.Forms.Padding(8, 4, 8, 4);
            this.lblInfo.Size = new System.Drawing.Size(514, 28);
            this.lblInfo.TabIndex = 5;
            this.lblInfo.Text = "You must restart Visual Studio in order for the changes to take effect.";
            // 
            // GeneralOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(9F, 20F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grbOptions);
            this.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.Name = "GeneralOptionsControl";
            this.Size = new System.Drawing.Size(689, 498);
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
    }
}

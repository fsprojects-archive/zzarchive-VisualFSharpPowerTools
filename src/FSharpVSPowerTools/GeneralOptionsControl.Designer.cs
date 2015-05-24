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
            this.chbQuickInfoPanel = new System.Windows.Forms.CheckBox();
            this.chbGoToSymbolSource = new System.Windows.Forms.CheckBox();
            this.chbGenerateReferences = new System.Windows.Forms.CheckBox();
            this.chbUnusedOpens = new System.Windows.Forms.CheckBox();
            this.chbTaskListComments = new System.Windows.Forms.CheckBox();
            this.chbGoToMetadata = new System.Windows.Forms.CheckBox();
            this.chbUnusedReferences = new System.Windows.Forms.CheckBox();
            this.chbResolveUnopenedNamespaces = new System.Windows.Forms.CheckBox();
            this.chbUnionPatternMatchCaseGeneration = new System.Windows.Forms.CheckBox();
            this.chbRecordStubGeneration = new System.Windows.Forms.CheckBox();
            this.chbFindAllReferences = new System.Windows.Forms.CheckBox();
            this.lblInformation = new System.Windows.Forms.Label();
            this.chbInterfaceImplementation = new System.Windows.Forms.CheckBox();
            this.chbFolderOrganization = new System.Windows.Forms.CheckBox();
            this.chbSyntaxColoring = new System.Windows.Forms.CheckBox();
            this.chbNavigateTo = new System.Windows.Forms.CheckBox();
            this.chbDepthColorizer = new System.Windows.Forms.CheckBox();
            this.chbRenameRefactoring = new System.Windows.Forms.CheckBox();
            this.chbHighlightUsage = new System.Windows.Forms.CheckBox();
            this.chbNavBar = new System.Windows.Forms.CheckBox();
            this.lblInfo = new System.Windows.Forms.Label();
            this.chbFormatting = new System.Windows.Forms.CheckBox();
            this.chbXmlDoc = new System.Windows.Forms.CheckBox();
            this.grbOptions.SuspendLayout();
            this.SuspendLayout();
            // 
            // grbOptions
            // 
            this.grbOptions.Controls.Add(this.chbQuickInfoPanel);
            this.grbOptions.Controls.Add(this.chbGoToSymbolSource);
            this.grbOptions.Controls.Add(this.chbGenerateReferences);
            this.grbOptions.Controls.Add(this.chbUnusedOpens);
            this.grbOptions.Controls.Add(this.chbTaskListComments);
            this.grbOptions.Controls.Add(this.chbGoToMetadata);
            this.grbOptions.Controls.Add(this.chbUnusedReferences);
            this.grbOptions.Controls.Add(this.chbResolveUnopenedNamespaces);
            this.grbOptions.Controls.Add(this.chbUnionPatternMatchCaseGeneration);
            this.grbOptions.Controls.Add(this.chbRecordStubGeneration);
            this.grbOptions.Controls.Add(this.chbFindAllReferences);
            this.grbOptions.Controls.Add(this.lblInformation);
            this.grbOptions.Controls.Add(this.chbInterfaceImplementation);
            this.grbOptions.Controls.Add(this.chbFolderOrganization);
            this.grbOptions.Controls.Add(this.chbSyntaxColoring);
            this.grbOptions.Controls.Add(this.chbNavigateTo);
            this.grbOptions.Controls.Add(this.chbDepthColorizer);
            this.grbOptions.Controls.Add(this.chbRenameRefactoring);
            this.grbOptions.Controls.Add(this.chbHighlightUsage);
            this.grbOptions.Controls.Add(this.chbNavBar);
            this.grbOptions.Controls.Add(this.lblInfo);
            this.grbOptions.Controls.Add(this.chbFormatting);
            this.grbOptions.Controls.Add(this.chbXmlDoc);
            this.grbOptions.Location = new System.Drawing.Point(3, 3);
            this.grbOptions.Name = "grbOptions";
            this.grbOptions.Size = new System.Drawing.Size(481, 317);
            this.grbOptions.TabIndex = 0;
            this.grbOptions.TabStop = false;
            this.grbOptions.Text = "Turn features on/off";
            // 
            // chbQuickInfoPanel
            // 
            this.chbQuickInfoPanel.AutoSize = true;
            this.chbQuickInfoPanel.Checked = true;
            this.chbQuickInfoPanel.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbQuickInfoPanel.Location = new System.Drawing.Point(213, 227);
            this.chbQuickInfoPanel.Name = "chbQuickInfoPanel";
            this.chbQuickInfoPanel.Size = new System.Drawing.Size(103, 17);
            this.chbQuickInfoPanel.TabIndex = 22;
            this.chbQuickInfoPanel.Text = "Quick info panel";
            this.chbQuickInfoPanel.UseVisualStyleBackColor = true;
            // 
            // chbGoToSymbolSource
            // 
            this.chbGoToSymbolSource.AutoSize = true;
            this.chbGoToSymbolSource.Checked = true;
            this.chbGoToSymbolSource.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGoToSymbolSource.Location = new System.Drawing.Point(213, 204);
            this.chbGoToSymbolSource.Name = "chbGoToSymbolSource";
            this.chbGoToSymbolSource.Size = new System.Drawing.Size(116, 17);
            this.chbGoToSymbolSource.TabIndex = 21;
            this.chbGoToSymbolSource.Text = "Navigate to source";
            this.chbGoToSymbolSource.UseVisualStyleBackColor = true;
            // 
            // chbGenerateReferences
            // 
            this.chbGenerateReferences.AutoSize = true;
            this.chbGenerateReferences.Checked = true;
            this.chbGenerateReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGenerateReferences.Location = new System.Drawing.Point(213, 181);
            this.chbGenerateReferences.Name = "chbGenerateReferences";
            this.chbGenerateReferences.Size = new System.Drawing.Size(157, 17);
            this.chbGenerateReferences.TabIndex = 20;
            this.chbGenerateReferences.Text = "Generate references for FSI";
            this.chbGenerateReferences.UseVisualStyleBackColor = true;
            // 
            // chbUnusedOpens
            // 
            this.chbUnusedOpens.AutoSize = true;
            this.chbUnusedOpens.Checked = true;
            this.chbUnusedOpens.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnusedOpens.Location = new System.Drawing.Point(34, 227);
            this.chbUnusedOpens.Name = "chbUnusedOpens";
            this.chbUnusedOpens.Size = new System.Drawing.Size(136, 17);
            this.chbUnusedOpens.TabIndex = 19;
            this.chbUnusedOpens.Text = "Gray out unused opens";
            this.chbUnusedOpens.UseVisualStyleBackColor = true;
            // 
            // chbTaskListComments
            // 
            this.chbTaskListComments.AutoSize = true;
            this.chbTaskListComments.Checked = true;
            this.chbTaskListComments.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbTaskListComments.Location = new System.Drawing.Point(213, 157);
            this.chbTaskListComments.Name = "chbTaskListComments";
            this.chbTaskListComments.Size = new System.Drawing.Size(120, 17);
            this.chbTaskListComments.TabIndex = 18;
            this.chbTaskListComments.Text = "Task List comments";
            this.chbTaskListComments.UseVisualStyleBackColor = true;
            // 
            // chbGoToMetadata
            // 
            this.chbGoToMetadata.AutoSize = true;
            this.chbGoToMetadata.Checked = true;
            this.chbGoToMetadata.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGoToMetadata.Location = new System.Drawing.Point(213, 134);
            this.chbGoToMetadata.Name = "chbGoToMetadata";
            this.chbGoToMetadata.Size = new System.Drawing.Size(99, 17);
            this.chbGoToMetadata.TabIndex = 18;
            this.chbGoToMetadata.Text = "Go to metadata";
            this.chbGoToMetadata.UseVisualStyleBackColor = true;
            // 
            // chbUnusedReferences
            // 
            this.chbUnusedReferences.AutoSize = true;
            this.chbUnusedReferences.Checked = true;
            this.chbUnusedReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnusedReferences.Location = new System.Drawing.Point(34, 204);
            this.chbUnusedReferences.Name = "chbUnusedReferences";
            this.chbUnusedReferences.Size = new System.Drawing.Size(164, 17);
            this.chbUnusedReferences.TabIndex = 17;
            this.chbUnusedReferences.Text = "Gray out unused declarations";
            this.chbUnusedReferences.UseVisualStyleBackColor = true;
            // 
            // chbResolveUnopenedNamespaces
            // 
            this.chbResolveUnopenedNamespaces.AutoSize = true;
            this.chbResolveUnopenedNamespaces.Checked = true;
            this.chbResolveUnopenedNamespaces.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbResolveUnopenedNamespaces.Location = new System.Drawing.Point(213, 110);
            this.chbResolveUnopenedNamespaces.Name = "chbResolveUnopenedNamespaces";
            this.chbResolveUnopenedNamespaces.Size = new System.Drawing.Size(179, 17);
            this.chbResolveUnopenedNamespaces.TabIndex = 16;
            this.chbResolveUnopenedNamespaces.Text = "Resolve unopened namespaces";
            this.chbResolveUnopenedNamespaces.UseVisualStyleBackColor = true;
            // 
            // chbUnionPatternMatchCaseGeneration
            // 
            this.chbUnionPatternMatchCaseGeneration.AutoSize = true;
            this.chbUnionPatternMatchCaseGeneration.Checked = true;
            this.chbUnionPatternMatchCaseGeneration.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnionPatternMatchCaseGeneration.Location = new System.Drawing.Point(213, 86);
            this.chbUnionPatternMatchCaseGeneration.Name = "chbUnionPatternMatchCaseGeneration";
            this.chbUnionPatternMatchCaseGeneration.Size = new System.Drawing.Size(201, 17);
            this.chbUnionPatternMatchCaseGeneration.TabIndex = 16;
            this.chbUnionPatternMatchCaseGeneration.Text = "Union pattern match case generation";
            this.chbUnionPatternMatchCaseGeneration.UseVisualStyleBackColor = true;
            // 
            // chbRecordStubGeneration
            // 
            this.chbRecordStubGeneration.AutoSize = true;
            this.chbRecordStubGeneration.Checked = true;
            this.chbRecordStubGeneration.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbRecordStubGeneration.Location = new System.Drawing.Point(213, 63);
            this.chbRecordStubGeneration.Name = "chbRecordStubGeneration";
            this.chbRecordStubGeneration.Size = new System.Drawing.Size(137, 17);
            this.chbRecordStubGeneration.TabIndex = 15;
            this.chbRecordStubGeneration.Text = "Record stub generation";
            this.chbRecordStubGeneration.UseVisualStyleBackColor = true;
            // 
            // chbFindAllReferences
            // 
            this.chbFindAllReferences.AutoSize = true;
            this.chbFindAllReferences.Checked = true;
            this.chbFindAllReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbFindAllReferences.Location = new System.Drawing.Point(213, 16);
            this.chbFindAllReferences.Name = "chbFindAllReferences";
            this.chbFindAllReferences.Size = new System.Drawing.Size(112, 17);
            this.chbFindAllReferences.TabIndex = 14;
            this.chbFindAllReferences.Text = "Find all references";
            this.chbFindAllReferences.UseVisualStyleBackColor = true;
            // 
            // lblInformation
            // 
            this.lblInformation.AutoSize = true;
            this.lblInformation.Location = new System.Drawing.Point(12, 274);
            this.lblInformation.Name = "lblInformation";
            this.lblInformation.Size = new System.Drawing.Size(139, 13);
            this.lblInformation.TabIndex = 13;
            this.lblInformation.Text = "*** Admin privileges required";
            // 
            // chbInterfaceImplementation
            // 
            this.chbInterfaceImplementation.AutoSize = true;
            this.chbInterfaceImplementation.Checked = true;
            this.chbInterfaceImplementation.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbInterfaceImplementation.Location = new System.Drawing.Point(213, 39);
            this.chbInterfaceImplementation.Name = "chbInterfaceImplementation";
            this.chbInterfaceImplementation.Size = new System.Drawing.Size(118, 17);
            this.chbInterfaceImplementation.TabIndex = 12;
            this.chbInterfaceImplementation.Text = "Implement interface";
            this.chbInterfaceImplementation.UseVisualStyleBackColor = true;
            // 
            // chbFolderOrganization
            // 
            this.chbFolderOrganization.AutoSize = true;
            this.chbFolderOrganization.Location = new System.Drawing.Point(16, 250);
            this.chbFolderOrganization.Name = "chbFolderOrganization";
            this.chbFolderOrganization.Size = new System.Drawing.Size(115, 17);
            this.chbFolderOrganization.TabIndex = 12;
            this.chbFolderOrganization.Text = "Folder organization";
            this.chbFolderOrganization.UseVisualStyleBackColor = true;
            // 
            // chbSyntaxColoring
            // 
            this.chbSyntaxColoring.AutoSize = true;
            this.chbSyntaxColoring.Checked = true;
            this.chbSyntaxColoring.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbSyntaxColoring.Location = new System.Drawing.Point(16, 180);
            this.chbSyntaxColoring.Name = "chbSyntaxColoring";
            this.chbSyntaxColoring.Size = new System.Drawing.Size(98, 17);
            this.chbSyntaxColoring.TabIndex = 11;
            this.chbSyntaxColoring.Text = "Syntax coloring";
            this.chbSyntaxColoring.UseVisualStyleBackColor = true;
            // 
            // chbNavigateTo
            // 
            this.chbNavigateTo.AutoSize = true;
            this.chbNavigateTo.Checked = true;
            this.chbNavigateTo.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbNavigateTo.Location = new System.Drawing.Point(16, 157);
            this.chbNavigateTo.Name = "chbNavigateTo";
            this.chbNavigateTo.Size = new System.Drawing.Size(82, 17);
            this.chbNavigateTo.TabIndex = 10;
            this.chbNavigateTo.Text = "NavigateTo";
            this.chbNavigateTo.UseVisualStyleBackColor = true;
            // 
            // chbDepthColorizer
            // 
            this.chbDepthColorizer.AutoSize = true;
            this.chbDepthColorizer.Location = new System.Drawing.Point(16, 134);
            this.chbDepthColorizer.Name = "chbDepthColorizer";
            this.chbDepthColorizer.Size = new System.Drawing.Size(166, 17);
            this.chbDepthColorizer.TabIndex = 9;
            this.chbDepthColorizer.Text = "Indent guides/Depth colorizer";
            this.chbDepthColorizer.UseVisualStyleBackColor = true;
            // 
            // chbRenameRefactoring
            // 
            this.chbRenameRefactoring.AutoSize = true;
            this.chbRenameRefactoring.Checked = true;
            this.chbRenameRefactoring.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbRenameRefactoring.Location = new System.Drawing.Point(16, 110);
            this.chbRenameRefactoring.Name = "chbRenameRefactoring";
            this.chbRenameRefactoring.Size = new System.Drawing.Size(119, 17);
            this.chbRenameRefactoring.TabIndex = 8;
            this.chbRenameRefactoring.Text = "Rename refactoring";
            this.chbRenameRefactoring.UseVisualStyleBackColor = true;
            // 
            // chbHighlightUsage
            // 
            this.chbHighlightUsage.AutoSize = true;
            this.chbHighlightUsage.Checked = true;
            this.chbHighlightUsage.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbHighlightUsage.Location = new System.Drawing.Point(16, 87);
            this.chbHighlightUsage.Name = "chbHighlightUsage";
            this.chbHighlightUsage.Size = new System.Drawing.Size(120, 17);
            this.chbHighlightUsage.TabIndex = 7;
            this.chbHighlightUsage.Text = "Highlight references";
            this.chbHighlightUsage.UseVisualStyleBackColor = true;
            // 
            // chbNavBar
            // 
            this.chbNavBar.AutoSize = true;
            this.chbNavBar.Location = new System.Drawing.Point(16, 63);
            this.chbNavBar.Name = "chbNavBar";
            this.chbNavBar.Size = new System.Drawing.Size(110, 17);
            this.chbNavBar.TabIndex = 6;
            this.chbNavBar.Text = "Navigation bar ***";
            this.chbNavBar.UseVisualStyleBackColor = true;
            // 
            // lblInfo
            // 
            this.lblInfo.AutoSize = true;
            this.lblInfo.ForeColor = System.Drawing.SystemColors.HotTrack;
            this.lblInfo.Location = new System.Drawing.Point(13, 291);
            this.lblInfo.Name = "lblInfo";
            this.lblInfo.Size = new System.Drawing.Size(331, 13);
            this.lblInfo.TabIndex = 5;
            this.lblInfo.Text = "You must restart Visual Studio in order for the changes to take effect.";
            // 
            // chbFormatting
            // 
            this.chbFormatting.AutoSize = true;
            this.chbFormatting.Checked = true;
            this.chbFormatting.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbFormatting.Location = new System.Drawing.Point(16, 40);
            this.chbFormatting.Name = "chbFormatting";
            this.chbFormatting.Size = new System.Drawing.Size(136, 17);
            this.chbFormatting.TabIndex = 2;
            this.chbFormatting.Text = "Source code formatting";
            this.chbFormatting.UseVisualStyleBackColor = true;
            // 
            // chbXmlDoc
            // 
            this.chbXmlDoc.AutoSize = true;
            this.chbXmlDoc.Checked = true;
            this.chbXmlDoc.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbXmlDoc.Location = new System.Drawing.Point(16, 16);
            this.chbXmlDoc.Name = "chbXmlDoc";
            this.chbXmlDoc.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.chbXmlDoc.Size = new System.Drawing.Size(141, 17);
            this.chbXmlDoc.TabIndex = 1;
            this.chbXmlDoc.Text = "Auto-generating XmlDoc";
            this.chbXmlDoc.UseVisualStyleBackColor = true;
            // 
            // GeneralOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grbOptions);
            this.Name = "GeneralOptionsControl";
            this.Size = new System.Drawing.Size(559, 333);
            this.Load += new System.EventHandler(this.GeneralOptionsControl_Load);
            this.grbOptions.ResumeLayout(false);
            this.grbOptions.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox grbOptions;
        private System.Windows.Forms.CheckBox chbFormatting;
        private System.Windows.Forms.CheckBox chbXmlDoc;
        private System.Windows.Forms.Label lblInfo;
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
    }
}

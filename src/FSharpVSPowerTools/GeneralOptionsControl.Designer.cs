﻿using System.Drawing;

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
            this.lblTitle = new System.Windows.Forms.Label();
            this.lblHome = new System.Windows.Forms.LinkLabel();
            this.chbFormatting = new System.Windows.Forms.CheckBox();
            this.chbXmlDoc = new System.Windows.Forms.CheckBox();
            this.chbGenerateReferences = new System.Windows.Forms.CheckBox();
            this.chbCSharpTypos = new System.Windows.Forms.CheckBox();
            this.grbOptions.SuspendLayout();
            this.SuspendLayout();
            // 
            // grbOptions
            // 
            this.grbOptions.Controls.Add(this.chbCSharpTypos);
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
            this.grbOptions.Controls.Add(this.lblTitle);
            this.grbOptions.Controls.Add(this.lblHome);
            this.grbOptions.Controls.Add(this.chbFormatting);
            this.grbOptions.Controls.Add(this.chbXmlDoc);
            this.grbOptions.Location = new System.Drawing.Point(4, 4);
            this.grbOptions.Margin = new System.Windows.Forms.Padding(4);
            this.grbOptions.Name = "grbOptions";
            this.grbOptions.Padding = new System.Windows.Forms.Padding(4);
            this.grbOptions.Size = new System.Drawing.Size(641, 375);
            this.grbOptions.TabIndex = 0;
            this.grbOptions.TabStop = false;
            this.grbOptions.Text = "Turn features on/off";
            // 
            // chbUnusedOpens
            // 
            this.chbUnusedOpens.AutoSize = true;
            this.chbUnusedOpens.Checked = true;
            this.chbUnusedOpens.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnusedOpens.Location = new System.Drawing.Point(272, 192);
            this.chbUnusedOpens.Margin = new System.Windows.Forms.Padding(4);
            this.chbUnusedOpens.Name = "chbUnusedOpens";
            this.chbUnusedOpens.Size = new System.Drawing.Size(179, 21);
            this.chbUnusedOpens.TabIndex = 19;
            this.chbUnusedOpens.Text = "Gray out unused opens";
            this.chbUnusedOpens.UseVisualStyleBackColor = true;
            // 
            // chbTaskListComments
            // 
            this.chbTaskListComments.AutoSize = true;
            this.chbTaskListComments.Checked = true;
            this.chbTaskListComments.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbTaskListComments.Location = new System.Drawing.Point(272, 252);
            this.chbTaskListComments.Margin = new System.Windows.Forms.Padding(4);
            this.chbTaskListComments.Name = "chbTaskListComments";
            this.chbTaskListComments.Size = new System.Drawing.Size(155, 21);
            this.chbTaskListComments.TabIndex = 18;
            this.chbTaskListComments.Text = "Task List comments";
            this.chbTaskListComments.UseVisualStyleBackColor = true;
            // 
            // chbGoToMetadata
            // 
            this.chbGoToMetadata.AutoSize = true;
            this.chbGoToMetadata.Checked = true;
            this.chbGoToMetadata.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGoToMetadata.Location = new System.Drawing.Point(272, 223);
            this.chbGoToMetadata.Margin = new System.Windows.Forms.Padding(4);
            this.chbGoToMetadata.Name = "chbGoToMetadata";
            this.chbGoToMetadata.Size = new System.Drawing.Size(128, 21);
            this.chbGoToMetadata.TabIndex = 18;
            this.chbGoToMetadata.Text = "Go to metadata";
            this.chbGoToMetadata.UseVisualStyleBackColor = true;
            // 
            // chbUnusedReferences
            // 
            this.chbUnusedReferences.AutoSize = true;
            this.chbUnusedReferences.Checked = true;
            this.chbUnusedReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnusedReferences.Location = new System.Drawing.Point(272, 164);
            this.chbUnusedReferences.Margin = new System.Windows.Forms.Padding(4);
            this.chbUnusedReferences.Name = "chbUnusedReferences";
            this.chbUnusedReferences.Size = new System.Drawing.Size(217, 21);
            this.chbUnusedReferences.TabIndex = 17;
            this.chbUnusedReferences.Text = "Gray out unused declarations";
            this.chbUnusedReferences.UseVisualStyleBackColor = true;
            // 
            // chbResolveUnopenedNamespaces
            // 
            this.chbResolveUnopenedNamespaces.AutoSize = true;
            this.chbResolveUnopenedNamespaces.Checked = true;
            this.chbResolveUnopenedNamespaces.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbResolveUnopenedNamespaces.Location = new System.Drawing.Point(272, 135);
            this.chbResolveUnopenedNamespaces.Margin = new System.Windows.Forms.Padding(4);
            this.chbResolveUnopenedNamespaces.Name = "chbResolveUnopenedNamespaces";
            this.chbResolveUnopenedNamespaces.Size = new System.Drawing.Size(233, 21);
            this.chbResolveUnopenedNamespaces.TabIndex = 16;
            this.chbResolveUnopenedNamespaces.Text = "Resolve unopened namespaces";
            this.chbResolveUnopenedNamespaces.UseVisualStyleBackColor = true;
            // 
            // chbUnionPatternMatchCaseGeneration
            // 
            this.chbUnionPatternMatchCaseGeneration.AutoSize = true;
            this.chbUnionPatternMatchCaseGeneration.Checked = true;
            this.chbUnionPatternMatchCaseGeneration.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbUnionPatternMatchCaseGeneration.Location = new System.Drawing.Point(272, 106);
            this.chbUnionPatternMatchCaseGeneration.Margin = new System.Windows.Forms.Padding(4);
            this.chbUnionPatternMatchCaseGeneration.Name = "chbUnionPatternMatchCaseGeneration";
            this.chbUnionPatternMatchCaseGeneration.Size = new System.Drawing.Size(264, 21);
            this.chbUnionPatternMatchCaseGeneration.TabIndex = 16;
            this.chbUnionPatternMatchCaseGeneration.Text = "Union pattern match case generation";
            this.chbUnionPatternMatchCaseGeneration.UseVisualStyleBackColor = true;
            // 
            // chbRecordStubGeneration
            // 
            this.chbRecordStubGeneration.AutoSize = true;
            this.chbRecordStubGeneration.Checked = true;
            this.chbRecordStubGeneration.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbRecordStubGeneration.Location = new System.Drawing.Point(272, 78);
            this.chbRecordStubGeneration.Margin = new System.Windows.Forms.Padding(4);
            this.chbRecordStubGeneration.Name = "chbRecordStubGeneration";
            this.chbRecordStubGeneration.Size = new System.Drawing.Size(179, 21);
            this.chbRecordStubGeneration.TabIndex = 15;
            this.chbRecordStubGeneration.Text = "Record stub generation";
            this.chbRecordStubGeneration.UseVisualStyleBackColor = true;
            // 
            // chbFindAllReferences
            // 
            this.chbFindAllReferences.AutoSize = true;
            this.chbFindAllReferences.Checked = true;
            this.chbFindAllReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbFindAllReferences.Location = new System.Drawing.Point(272, 20);
            this.chbFindAllReferences.Margin = new System.Windows.Forms.Padding(4);
            this.chbFindAllReferences.Name = "chbFindAllReferences";
            this.chbFindAllReferences.Size = new System.Drawing.Size(147, 21);
            this.chbFindAllReferences.TabIndex = 14;
            this.chbFindAllReferences.Text = "Find all references";
            this.chbFindAllReferences.UseVisualStyleBackColor = true;
            // 
            // lblInformation
            // 
            this.lblInformation.AutoSize = true;
            this.lblInformation.Location = new System.Drawing.Point(14, 313);
            this.lblInformation.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblInformation.Name = "lblInformation";
            this.lblInformation.Size = new System.Drawing.Size(187, 17);
            this.lblInformation.TabIndex = 13;
            this.lblInformation.Text = "*** Admin privileges required";
            // 
            // chbInterfaceImplementation
            // 
            this.chbInterfaceImplementation.AutoSize = true;
            this.chbInterfaceImplementation.Checked = true;
            this.chbInterfaceImplementation.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbInterfaceImplementation.Location = new System.Drawing.Point(272, 48);
            this.chbInterfaceImplementation.Margin = new System.Windows.Forms.Padding(4);
            this.chbInterfaceImplementation.Name = "chbInterfaceImplementation";
            this.chbInterfaceImplementation.Size = new System.Drawing.Size(153, 21);
            this.chbInterfaceImplementation.TabIndex = 12;
            this.chbInterfaceImplementation.Text = "Implement interface";
            this.chbInterfaceImplementation.UseVisualStyleBackColor = true;
            // 
            // chbFolderOrganization
            // 
            this.chbFolderOrganization.AutoSize = true;
            this.chbFolderOrganization.Location = new System.Drawing.Point(20, 251);
            this.chbFolderOrganization.Margin = new System.Windows.Forms.Padding(4);
            this.chbFolderOrganization.Name = "chbFolderOrganization";
            this.chbFolderOrganization.Size = new System.Drawing.Size(152, 21);
            this.chbFolderOrganization.TabIndex = 12;
            this.chbFolderOrganization.Text = "Folder organization";
            this.chbFolderOrganization.UseVisualStyleBackColor = true;
            // 
            // chbSyntaxColoring
            // 
            this.chbSyntaxColoring.AutoSize = true;
            this.chbSyntaxColoring.Checked = true;
            this.chbSyntaxColoring.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbSyntaxColoring.Location = new System.Drawing.Point(20, 223);
            this.chbSyntaxColoring.Margin = new System.Windows.Forms.Padding(4);
            this.chbSyntaxColoring.Name = "chbSyntaxColoring";
            this.chbSyntaxColoring.Size = new System.Drawing.Size(126, 21);
            this.chbSyntaxColoring.TabIndex = 11;
            this.chbSyntaxColoring.Text = "Syntax coloring";
            this.chbSyntaxColoring.UseVisualStyleBackColor = true;
            // 
            // chbNavigateTo
            // 
            this.chbNavigateTo.AutoSize = true;
            this.chbNavigateTo.Checked = true;
            this.chbNavigateTo.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbNavigateTo.Location = new System.Drawing.Point(21, 193);
            this.chbNavigateTo.Margin = new System.Windows.Forms.Padding(4);
            this.chbNavigateTo.Name = "chbNavigateTo";
            this.chbNavigateTo.Size = new System.Drawing.Size(103, 21);
            this.chbNavigateTo.TabIndex = 10;
            this.chbNavigateTo.Text = "NavigateTo";
            this.chbNavigateTo.UseVisualStyleBackColor = true;
            // 
            // chbDepthColorizer
            // 
            this.chbDepthColorizer.AutoSize = true;
            this.chbDepthColorizer.Location = new System.Drawing.Point(21, 165);
            this.chbDepthColorizer.Margin = new System.Windows.Forms.Padding(4);
            this.chbDepthColorizer.Name = "chbDepthColorizer";
            this.chbDepthColorizer.Size = new System.Drawing.Size(215, 21);
            this.chbDepthColorizer.TabIndex = 9;
            this.chbDepthColorizer.Text = "Indent guides/Depth colorizer";
            this.chbDepthColorizer.UseVisualStyleBackColor = true;
            // 
            // chbRenameRefactoring
            // 
            this.chbRenameRefactoring.AutoSize = true;
            this.chbRenameRefactoring.Checked = true;
            this.chbRenameRefactoring.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbRenameRefactoring.Location = new System.Drawing.Point(21, 135);
            this.chbRenameRefactoring.Margin = new System.Windows.Forms.Padding(4);
            this.chbRenameRefactoring.Name = "chbRenameRefactoring";
            this.chbRenameRefactoring.Size = new System.Drawing.Size(155, 21);
            this.chbRenameRefactoring.TabIndex = 8;
            this.chbRenameRefactoring.Text = "Rename refactoring";
            this.chbRenameRefactoring.UseVisualStyleBackColor = true;
            // 
            // chbHighlightUsage
            // 
            this.chbHighlightUsage.AutoSize = true;
            this.chbHighlightUsage.Checked = true;
            this.chbHighlightUsage.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbHighlightUsage.Location = new System.Drawing.Point(21, 107);
            this.chbHighlightUsage.Margin = new System.Windows.Forms.Padding(4);
            this.chbHighlightUsage.Name = "chbHighlightUsage";
            this.chbHighlightUsage.Size = new System.Drawing.Size(157, 21);
            this.chbHighlightUsage.TabIndex = 7;
            this.chbHighlightUsage.Text = "Highlight references";
            this.chbHighlightUsage.UseVisualStyleBackColor = true;
            // 
            // chbNavBar
            // 
            this.chbNavBar.AutoSize = true;
            this.chbNavBar.Location = new System.Drawing.Point(21, 78);
            this.chbNavBar.Margin = new System.Windows.Forms.Padding(4);
            this.chbNavBar.Name = "chbNavBar";
            this.chbNavBar.Size = new System.Drawing.Size(141, 21);
            this.chbNavBar.TabIndex = 6;
            this.chbNavBar.Text = "Navigation bar ***";
            this.chbNavBar.UseVisualStyleBackColor = true;
            // 
            // lblInfo
            // 
            this.lblInfo.AutoSize = true;
            this.lblInfo.ForeColor = System.Drawing.SystemColors.HotTrack;
            this.lblInfo.Location = new System.Drawing.Point(15, 334);
            this.lblInfo.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblInfo.Name = "lblInfo";
            this.lblInfo.Size = new System.Drawing.Size(444, 17);
            this.lblInfo.TabIndex = 5;
            this.lblInfo.Text = "You must restart Visual Studio in order for the changes to take effect.";
            // 
            // lblTitle
            // 
            this.lblTitle.AutoSize = true;
            this.lblTitle.Location = new System.Drawing.Point(15, 354);
            this.lblTitle.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblTitle.Name = "lblTitle";
            this.lblTitle.Size = new System.Drawing.Size(128, 17);
            this.lblTitle.TabIndex = 4;
            this.lblTitle.Text = "Visit user guides at";
            // 
            // lblHome
            // 
            this.lblHome.AutoSize = true;
            this.lblHome.Location = new System.Drawing.Point(150, 354);
            this.lblHome.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblHome.Name = "lblHome";
            this.lblHome.Size = new System.Drawing.Size(329, 17);
            this.lblHome.TabIndex = 3;
            this.lblHome.TabStop = true;
            this.lblHome.Text = "http://fsprojects.github.io/VisualFSharpPowerTools/";
            this.lblHome.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.lblHome_LinkClicked);
            // 
            // chbFormatting
            // 
            this.chbFormatting.AutoSize = true;
            this.chbFormatting.Checked = true;
            this.chbFormatting.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbFormatting.Location = new System.Drawing.Point(21, 49);
            this.chbFormatting.Margin = new System.Windows.Forms.Padding(4);
            this.chbFormatting.Name = "chbFormatting";
            this.chbFormatting.Size = new System.Drawing.Size(177, 21);
            this.chbFormatting.TabIndex = 2;
            this.chbFormatting.Text = "Source code formatting";
            this.chbFormatting.UseVisualStyleBackColor = true;
            // 
            // chbXmlDoc
            // 
            this.chbXmlDoc.AutoSize = true;
            this.chbXmlDoc.Checked = true;
            this.chbXmlDoc.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbXmlDoc.Location = new System.Drawing.Point(21, 20);
            this.chbXmlDoc.Margin = new System.Windows.Forms.Padding(4);
            this.chbXmlDoc.Name = "chbXmlDoc";
            this.chbXmlDoc.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.chbXmlDoc.Size = new System.Drawing.Size(184, 21);
            this.chbXmlDoc.TabIndex = 1;
            this.chbXmlDoc.Text = "Auto-generating XmlDoc";
            this.chbXmlDoc.UseVisualStyleBackColor = true;
            // 
            // chbGenerateReferences
            // 
            this.chbGenerateReferences.AutoSize = true;
            this.chbGenerateReferences.Checked = true;
            this.chbGenerateReferences.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbGenerateReferences.Location = new System.Drawing.Point(272, 281);
            this.chbGenerateReferences.Margin = new System.Windows.Forms.Padding(4);
            this.chbGenerateReferences.Name = "chbGenerateReferences";
            this.chbGenerateReferences.Size = new System.Drawing.Size(207, 21);
            this.chbGenerateReferences.TabIndex = 20;
            this.chbGenerateReferences.Text = "Generate references for FSI";
            this.chbGenerateReferences.UseVisualStyleBackColor = true;
            // 
            // chbCSharpTypos
            // 
            this.chbCSharpTypos.AutoSize = true;
            this.chbCSharpTypos.Checked = true;
            this.chbCSharpTypos.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbCSharpTypos.Location = new System.Drawing.Point(272, 309);
            this.chbCSharpTypos.Margin = new System.Windows.Forms.Padding(4);
            this.chbCSharpTypos.Name = "chbCSharpTypos";
            this.chbCSharpTypos.Size = new System.Drawing.Size(180, 21);
            this.chbCSharpTypos.TabIndex = 21;
            this.chbCSharpTypos.Text = "Correct typical C#-typos";
            this.chbCSharpTypos.UseVisualStyleBackColor = true;
            // 
            // GeneralOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grbOptions);
            this.Margin = new System.Windows.Forms.Padding(4);
            this.Name = "GeneralOptionsControl";
            this.Size = new System.Drawing.Size(745, 396);
            this.Load += new System.EventHandler(this.GeneralOptionsControl_Load);
            this.grbOptions.ResumeLayout(false);
            this.grbOptions.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox grbOptions;
        private System.Windows.Forms.CheckBox chbFormatting;
        private System.Windows.Forms.CheckBox chbXmlDoc;
        private System.Windows.Forms.Label lblTitle;
        private System.Windows.Forms.LinkLabel lblHome;
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
        private System.Windows.Forms.CheckBox chbCSharpTypos;
    }
}

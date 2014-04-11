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
            this.chbSyntaxColoring = new System.Windows.Forms.CheckBox();
            this.chbFolderOrganization = new System.Windows.Forms.CheckBox();
            this.grbOptions.SuspendLayout();
            this.SuspendLayout();
            // 
            // grbOptions
            // 
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
            this.grbOptions.Location = new System.Drawing.Point(3, 3);
            this.grbOptions.Name = "grbOptions";
            this.grbOptions.Size = new System.Drawing.Size(424, 293);
            this.grbOptions.TabIndex = 0;
            this.grbOptions.TabStop = false;
            this.grbOptions.Text = "Turn features on/off";
            // 
            // chbNavigateTo
            // 
            this.chbNavigateTo.AutoSize = true;
            this.chbNavigateTo.Checked = true;
            this.chbNavigateTo.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbNavigateTo.Location = new System.Drawing.Point(16, 168);
            this.chbNavigateTo.Name = "chbNavigateTo";
            this.chbNavigateTo.Size = new System.Drawing.Size(82, 17);
            this.chbNavigateTo.TabIndex = 10;
            this.chbNavigateTo.Text = "NavigateTo";
            this.chbNavigateTo.UseVisualStyleBackColor = true;
            // 
            // chbDepthColorizer
            // 
            this.chbDepthColorizer.AutoSize = true;
            this.chbDepthColorizer.Location = new System.Drawing.Point(16, 145);
            this.chbDepthColorizer.Name = "chbDepthColorizer";
            this.chbDepthColorizer.Size = new System.Drawing.Size(97, 17);
            this.chbDepthColorizer.TabIndex = 9;
            this.chbDepthColorizer.Text = "Depth colorizer";
            this.chbDepthColorizer.UseVisualStyleBackColor = true;
            // 
            // chbRenameRefactoring
            // 
            this.chbRenameRefactoring.AutoSize = true;
            this.chbRenameRefactoring.Checked = true;
            this.chbRenameRefactoring.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbRenameRefactoring.Location = new System.Drawing.Point(16, 122);
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
            this.chbHighlightUsage.Location = new System.Drawing.Point(16, 99);
            this.chbHighlightUsage.Name = "chbHighlightUsage";
            this.chbHighlightUsage.Size = new System.Drawing.Size(120, 17);
            this.chbHighlightUsage.TabIndex = 7;
            this.chbHighlightUsage.Text = "Highlight references";
            this.chbHighlightUsage.UseVisualStyleBackColor = true;
            // 
            // chbNavBar
            // 
            this.chbNavBar.AutoSize = true;
            this.chbNavBar.Location = new System.Drawing.Point(16, 76);
            this.chbNavBar.Name = "chbNavBar";
            this.chbNavBar.Size = new System.Drawing.Size(221, 17);
            this.chbNavBar.TabIndex = 6;
            this.chbNavBar.Text = "Navigation bar (Admin privileges required)";
            this.chbNavBar.UseVisualStyleBackColor = true;
            // 
            // lblInfo
            // 
            this.lblInfo.AutoSize = true;
            this.lblInfo.ForeColor = System.Drawing.SystemColors.HotTrack;
            this.lblInfo.Location = new System.Drawing.Point(13, 255);
            this.lblInfo.Name = "lblInfo";
            this.lblInfo.Size = new System.Drawing.Size(331, 13);
            this.lblInfo.TabIndex = 5;
            this.lblInfo.Text = "You must restart Visual Studio in order for the changes to take effect.";
            // 
            // lblTitle
            // 
            this.lblTitle.AutoSize = true;
            this.lblTitle.Location = new System.Drawing.Point(13, 271);
            this.lblTitle.Name = "lblTitle";
            this.lblTitle.Size = new System.Drawing.Size(95, 13);
            this.lblTitle.TabIndex = 4;
            this.lblTitle.Text = "Visit user guides at";
            // 
            // lblHome
            // 
            this.lblHome.AutoSize = true;
            this.lblHome.Location = new System.Drawing.Point(101, 271);
            this.lblHome.Name = "lblHome";
            this.lblHome.Size = new System.Drawing.Size(254, 13);
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
            this.chbFormatting.Location = new System.Drawing.Point(16, 53);
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
            this.chbXmlDoc.Location = new System.Drawing.Point(16, 30);
            this.chbXmlDoc.Name = "chbXmlDoc";
            this.chbXmlDoc.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.chbXmlDoc.Size = new System.Drawing.Size(146, 17);
            this.chbXmlDoc.TabIndex = 1;
            this.chbXmlDoc.Text = "Auto-generating XMLDoc";
            this.chbXmlDoc.UseVisualStyleBackColor = true;
            // 
            // chbSyntaxColoring
            // 
            this.chbSyntaxColoring.AutoSize = true;
            this.chbSyntaxColoring.Checked = true;
            this.chbSyntaxColoring.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbSyntaxColoring.Location = new System.Drawing.Point(16, 191);
            this.chbSyntaxColoring.Name = "chbSyntaxColoring";
            this.chbSyntaxColoring.Size = new System.Drawing.Size(98, 17);
            this.chbSyntaxColoring.TabIndex = 11;
            this.chbSyntaxColoring.Text = "Syntax coloring";
            this.chbSyntaxColoring.UseVisualStyleBackColor = true;
            // 
            // chbFolderOrganization
            // 
            this.chbFolderOrganization.AutoSize = true;
            this.chbFolderOrganization.Location = new System.Drawing.Point(16, 214);
            this.chbFolderOrganization.Name = "chbFolderOrganization";
            this.chbFolderOrganization.Size = new System.Drawing.Size(115, 17);
            this.chbFolderOrganization.TabIndex = 12;
            this.chbFolderOrganization.Text = "Folder organization";
            this.chbFolderOrganization.UseVisualStyleBackColor = true;
            // 
            // GeneralOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grbOptions);
            this.Name = "GeneralOptionsControl";
            this.Size = new System.Drawing.Size(461, 310);
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
    }
}

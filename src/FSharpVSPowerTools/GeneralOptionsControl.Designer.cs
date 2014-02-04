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
            this.lblTitle = new System.Windows.Forms.Label();
            this.lblHome = new System.Windows.Forms.LinkLabel();
            this.chbFormatting = new System.Windows.Forms.CheckBox();
            this.chbXMLDoc = new System.Windows.Forms.CheckBox();
            this.grbOptions.SuspendLayout();
            this.SuspendLayout();
            // 
            // grbOptions
            // 
            this.grbOptions.Controls.Add(this.lblTitle);
            this.grbOptions.Controls.Add(this.lblHome);
            this.grbOptions.Controls.Add(this.chbFormatting);
            this.grbOptions.Controls.Add(this.chbXMLDoc);
            this.grbOptions.Location = new System.Drawing.Point(3, 3);
            this.grbOptions.Name = "grbOptions";
            this.grbOptions.Size = new System.Drawing.Size(414, 148);
            this.grbOptions.TabIndex = 0;
            this.grbOptions.TabStop = false;
            this.grbOptions.Text = "Features";
            // 
            // lblTitle
            // 
            this.lblTitle.AutoSize = true;
            this.lblTitle.Location = new System.Drawing.Point(13, 119);
            this.lblTitle.Name = "lblTitle";
            this.lblTitle.Size = new System.Drawing.Size(72, 13);
            this.lblTitle.TabIndex = 4;
            this.lblTitle.Text = "Learn more at";
            // 
            // lblHome
            // 
            this.lblHome.AutoSize = true;
            this.lblHome.Location = new System.Drawing.Point(84, 119);
            this.lblHome.Name = "lblHome";
            this.lblHome.Size = new System.Drawing.Size(240, 13);
            this.lblHome.TabIndex = 3;
            this.lblHome.TabStop = true;
            this.lblHome.Text = "http://fsprojects.github.io/FSharpVSPowerTools/";
            this.lblHome.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.lblHome_LinkClicked);
            // 
            // chbFormatting
            // 
            this.chbFormatting.AutoSize = true;
            this.chbFormatting.Checked = true;
            this.chbFormatting.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbFormatting.Location = new System.Drawing.Point(16, 53);
            this.chbFormatting.Name = "chbFormatting";
            this.chbFormatting.Size = new System.Drawing.Size(156, 17);
            this.chbFormatting.TabIndex = 2;
            this.chbFormatting.Text = "Use source code formatting";
            this.chbFormatting.UseVisualStyleBackColor = true;
            // 
            // chbXMLDoc
            // 
            this.chbXMLDoc.AutoSize = true;
            this.chbXMLDoc.Checked = true;
            this.chbXMLDoc.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chbXMLDoc.Location = new System.Drawing.Point(16, 30);
            this.chbXMLDoc.Name = "chbXMLDoc";
            this.chbXMLDoc.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.chbXMLDoc.Size = new System.Drawing.Size(165, 17);
            this.chbXMLDoc.TabIndex = 1;
            this.chbXMLDoc.Text = "Use auto-generated XMLDoc";
            this.chbXMLDoc.UseVisualStyleBackColor = true;
            // 
            // GeneralOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grbOptions);
            this.Name = "GeneralOptionsControl";
            this.Size = new System.Drawing.Size(454, 173);
            this.Load += new System.EventHandler(this.GeneralOptionsControl_Load);
            this.grbOptions.ResumeLayout(false);
            this.grbOptions.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox grbOptions;
        private System.Windows.Forms.CheckBox chbFormatting;
        private System.Windows.Forms.CheckBox chbXMLDoc;
        private System.Windows.Forms.Label lblTitle;
        private System.Windows.Forms.LinkLabel lblHome;
    }
}

namespace FSharpVSPowerTools.UI {
    partial class OutliningOptionControl {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing) {
            if (disposing && (components != null)) {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent() {
            this.flowLayoutPanel1 = new System.Windows.Forms.FlowLayoutPanel();
            this.lblOutliningGroup = new System.Windows.Forms.Label();
            this.cbEnabled = new System.Windows.Forms.CheckBox();
            this.cbCollapsedByDefault = new System.Windows.Forms.CheckBox();
            this.flowLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // flowLayoutPanel1
            // 
            this.flowLayoutPanel1.AutoSize = true;
            this.flowLayoutPanel1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.flowLayoutPanel1.Controls.Add(this.lblOutliningGroup);
            this.flowLayoutPanel1.Controls.Add(this.cbEnabled);
            this.flowLayoutPanel1.Controls.Add(this.cbCollapsedByDefault);
            this.flowLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.flowLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.flowLayoutPanel1.Margin = new System.Windows.Forms.Padding(0);
            this.flowLayoutPanel1.Name = "flowLayoutPanel1";
            this.flowLayoutPanel1.Size = new System.Drawing.Size(340, 17);
            this.flowLayoutPanel1.TabIndex = 0;
            this.flowLayoutPanel1.WrapContents = false;
            // 
            // lblOutliningGroup
            // 
            this.lblOutliningGroup.Location = new System.Drawing.Point(0, 0);
            this.lblOutliningGroup.Margin = new System.Windows.Forms.Padding(0);
            this.lblOutliningGroup.Name = "lblOutliningGroup";
            this.lblOutliningGroup.Size = new System.Drawing.Size(140, 15);
            this.lblOutliningGroup.TabIndex = 0;
            this.lblOutliningGroup.Text = "Outlining Group";
            this.lblOutliningGroup.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // cbEnabled
            // 
            this.cbEnabled.AutoSize = true;
            this.cbEnabled.Location = new System.Drawing.Point(143, 0);
            this.cbEnabled.Margin = new System.Windows.Forms.Padding(3, 0, 3, 0);
            this.cbEnabled.Name = "cbEnabled";
            this.cbEnabled.Size = new System.Drawing.Size(65, 17);
            this.cbEnabled.TabIndex = 1;
            this.cbEnabled.Text = "Enabled";
            this.cbEnabled.UseVisualStyleBackColor = true;
            // 
            // cbCollapsedByDefault
            // 
            this.cbCollapsedByDefault.AutoSize = true;
            this.cbCollapsedByDefault.Location = new System.Drawing.Point(214, 0);
            this.cbCollapsedByDefault.Margin = new System.Windows.Forms.Padding(3, 0, 3, 0);
            this.cbCollapsedByDefault.Name = "cbCollapsedByDefault";
            this.cbCollapsedByDefault.Size = new System.Drawing.Size(123, 17);
            this.cbCollapsedByDefault.TabIndex = 2;
            this.cbCollapsedByDefault.Text = "Collapsed by Default";
            this.cbCollapsedByDefault.UseVisualStyleBackColor = true;
            // 
            // OutliningOptionControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.Controls.Add(this.flowLayoutPanel1);
            this.Margin = new System.Windows.Forms.Padding(0);
            this.Name = "OutliningOptionControl";
            this.Size = new System.Drawing.Size(340, 17);
            this.flowLayoutPanel1.ResumeLayout(false);
            this.flowLayoutPanel1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;
        private System.Windows.Forms.Label lblOutliningGroup;
        private System.Windows.Forms.CheckBox cbEnabled;
        private System.Windows.Forms.CheckBox cbCollapsedByDefault;
    }
}

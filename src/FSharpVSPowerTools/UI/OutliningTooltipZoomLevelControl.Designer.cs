namespace FSharpVSPowerTools.UI {
    partial class OutliningTooltipZoomLevelControl {
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
            this.cb = new System.Windows.Forms.ComboBox();
            this.flowLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // flowLayoutPanel1
            // 
            this.flowLayoutPanel1.AutoSize = true;
            this.flowLayoutPanel1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.flowLayoutPanel1.Controls.Add(this.lblOutliningGroup);
            this.flowLayoutPanel1.Controls.Add(this.cb);
            this.flowLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.flowLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.flowLayoutPanel1.Margin = new System.Windows.Forms.Padding(0);
            this.flowLayoutPanel1.Name = "flowLayoutPanel1";
            this.flowLayoutPanel1.Size = new System.Drawing.Size(316, 24);
            this.flowLayoutPanel1.TabIndex = 0;
            this.flowLayoutPanel1.WrapContents = false;
            // 
            // lblOutliningGroup
            // 
            this.lblOutliningGroup.Location = new System.Drawing.Point(0, 0);
            this.lblOutliningGroup.Margin = new System.Windows.Forms.Padding(0);
            this.lblOutliningGroup.Name = "lblOutliningGroup";
            this.lblOutliningGroup.Size = new System.Drawing.Size(187, 18);
            this.lblOutliningGroup.TabIndex = 0;
            this.lblOutliningGroup.Text = "Tooltip zoom level";
            this.lblOutliningGroup.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // cb
            // 
            this.cb.ForeColor = System.Drawing.SystemColors.WindowText;
            this.cb.FormattingEnabled = true;
            this.cb.Location = new System.Drawing.Point(191, 0);
            this.cb.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.cb.Name = "cb";
            this.cb.Size = new System.Drawing.Size(121, 24);
            this.cb.TabIndex = 1;
            // 
            // OutliningTooltipZoomLevelControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.Controls.Add(this.flowLayoutPanel1);
            this.Margin = new System.Windows.Forms.Padding(0);
            this.Name = "OutliningTooltipZoomLevelControl";
            this.Size = new System.Drawing.Size(316, 24);
            this.flowLayoutPanel1.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;
        private System.Windows.Forms.Label lblOutliningGroup;
        private System.Windows.Forms.ComboBox cb;
    }
}

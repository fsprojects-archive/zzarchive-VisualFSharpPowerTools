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
            this.tb = new System.Windows.Forms.TextBox();
            this.flowLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // flowLayoutPanel1
            // 
            this.flowLayoutPanel1.AutoSize = true;
            this.flowLayoutPanel1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.flowLayoutPanel1.Controls.Add(this.lblOutliningGroup);
            this.flowLayoutPanel1.Controls.Add(this.tb);
            this.flowLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.flowLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.flowLayoutPanel1.Margin = new System.Windows.Forms.Padding(0);
            this.flowLayoutPanel1.Name = "flowLayoutPanel1";
            this.flowLayoutPanel1.Size = new System.Drawing.Size(295, 22);
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
            // tb
            // 
            this.tb.Location = new System.Drawing.Point(191, 0);
            this.tb.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.tb.Name = "tb";
            this.tb.Size = new System.Drawing.Size(100, 22);
            this.tb.TabIndex = 1;
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
            this.Size = new System.Drawing.Size(295, 22);
            this.flowLayoutPanel1.ResumeLayout(false);
            this.flowLayoutPanel1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;
        private System.Windows.Forms.Label lblOutliningGroup;
        private System.Windows.Forms.TextBox tb;
    }
}

namespace FSharpVSPowerTools.UI {
    partial class OutliningOptionsControl {
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
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            this.cbTopLevelCollapsedByDefault = new System.Windows.Forms.CheckBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.cbTopLevelEnabled = new System.Windows.Forms.CheckBox();
            this.cbMatchStatementEnabled = new System.Windows.Forms.CheckBox();
            this.cbMatchStatementCollapsedByDefault = new System.Windows.Forms.CheckBox();
            this.groupBox1.SuspendLayout();
            this.tableLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.tableLayoutPanel1);
            this.groupBox1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.groupBox1.Location = new System.Drawing.Point(0, 0);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(471, 241);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Outlining Options";
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.AutoSize = true;
            this.tableLayoutPanel1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.tableLayoutPanel1.ColumnCount = 3;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.Controls.Add(this.cbTopLevelCollapsedByDefault, 2, 0);
            this.tableLayoutPanel1.Controls.Add(this.label1, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.label2, 0, 1);
            this.tableLayoutPanel1.Controls.Add(this.cbTopLevelEnabled, 1, 0);
            this.tableLayoutPanel1.Controls.Add(this.cbMatchStatementEnabled, 1, 1);
            this.tableLayoutPanel1.Controls.Add(this.cbMatchStatementCollapsedByDefault, 2, 1);
            this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Top;
            this.tableLayoutPanel1.Location = new System.Drawing.Point(3, 16);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 2;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.Size = new System.Drawing.Size(465, 46);
            this.tableLayoutPanel1.TabIndex = 0;
            // 
            // cbTopLevelCollapsedByDefault
            // 
            this.cbTopLevelCollapsedByDefault.AutoSize = true;
            this.cbTopLevelCollapsedByDefault.Location = new System.Drawing.Point(180, 3);
            this.cbTopLevelCollapsedByDefault.Name = "cbTopLevelCollapsedByDefault";
            this.cbTopLevelCollapsedByDefault.Size = new System.Drawing.Size(124, 17);
            this.cbTopLevelCollapsedByDefault.TabIndex = 2;
            this.cbTopLevelCollapsedByDefault.Text = "Collapsed By Default";
            this.cbTopLevelCollapsedByDefault.UseVisualStyleBackColor = true;
            // 
            // label1
            // 
            this.label1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label1.Location = new System.Drawing.Point(3, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(100, 23);
            this.label1.TabIndex = 0;
            this.label1.Text = "Top Level:";
            this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // label2
            // 
            this.label2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label2.Location = new System.Drawing.Point(3, 23);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(100, 23);
            this.label2.TabIndex = 0;
            this.label2.Text = "Match Statement:";
            this.label2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // cbTopLevelEnabled
            // 
            this.cbTopLevelEnabled.AutoSize = true;
            this.cbTopLevelEnabled.Location = new System.Drawing.Point(109, 3);
            this.cbTopLevelEnabled.Name = "cbTopLevelEnabled";
            this.cbTopLevelEnabled.Size = new System.Drawing.Size(65, 17);
            this.cbTopLevelEnabled.TabIndex = 1;
            this.cbTopLevelEnabled.Text = "Enabled";
            this.cbTopLevelEnabled.UseVisualStyleBackColor = true;
            // 
            // cbMatchStatementEnabled
            // 
            this.cbMatchStatementEnabled.AutoSize = true;
            this.cbMatchStatementEnabled.Location = new System.Drawing.Point(109, 26);
            this.cbMatchStatementEnabled.Name = "cbMatchStatementEnabled";
            this.cbMatchStatementEnabled.Size = new System.Drawing.Size(65, 17);
            this.cbMatchStatementEnabled.TabIndex = 1;
            this.cbMatchStatementEnabled.Text = "Enabled";
            this.cbMatchStatementEnabled.UseVisualStyleBackColor = true;
            // 
            // cbMatchStatementCollapsedByDefault
            // 
            this.cbMatchStatementCollapsedByDefault.AutoSize = true;
            this.cbMatchStatementCollapsedByDefault.Location = new System.Drawing.Point(180, 26);
            this.cbMatchStatementCollapsedByDefault.Name = "cbMatchStatementCollapsedByDefault";
            this.cbMatchStatementCollapsedByDefault.Size = new System.Drawing.Size(124, 17);
            this.cbMatchStatementCollapsedByDefault.TabIndex = 2;
            this.cbMatchStatementCollapsedByDefault.Text = "Collapsed By Default";
            this.cbMatchStatementCollapsedByDefault.UseVisualStyleBackColor = true;
            // 
            // OutliningOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.groupBox1);
            this.Name = "OutliningOptionsControl";
            this.Size = new System.Drawing.Size(471, 241);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.tableLayoutPanel1.ResumeLayout(false);
            this.tableLayoutPanel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private System.Windows.Forms.CheckBox cbTopLevelCollapsedByDefault;
        private System.Windows.Forms.CheckBox cbTopLevelEnabled;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.CheckBox cbMatchStatementEnabled;
        private System.Windows.Forms.CheckBox cbMatchStatementCollapsedByDefault;
    }
}

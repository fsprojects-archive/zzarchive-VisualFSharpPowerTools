namespace FSharpVSPowerTools
{
    partial class CodeGenerationOptionsControl
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
            this.components = new System.ComponentModel.Container();
            this.grbBodyStyle = new System.Windows.Forms.GroupBox();
            this.tbxSourceCode = new System.Windows.Forms.TextBox();
            this.rdbUncompilable = new System.Windows.Forms.RadioButton();
            this.rdbDefaultValue = new System.Windows.Forms.RadioButton();
            this.rdbNotImplementedYet = new System.Windows.Forms.RadioButton();
            this.rdbFailwith = new System.Windows.Forms.RadioButton();
            this.ttpMain = new System.Windows.Forms.ToolTip(this.components);
            this.tblLayoutPanel = new System.Windows.Forms.TableLayoutPanel();
            this.grbBodyStyle.SuspendLayout();
            this.tblLayoutPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // grbBodyStyle
            // 
            this.grbBodyStyle.AutoSize = true;
            this.grbBodyStyle.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.grbBodyStyle.Controls.Add(this.tblLayoutPanel);
            this.grbBodyStyle.Dock = System.Windows.Forms.DockStyle.Top;
            this.grbBodyStyle.Location = new System.Drawing.Point(0, 0);
            this.grbBodyStyle.Margin = new System.Windows.Forms.Padding(0);
            this.grbBodyStyle.MinimumSize = new System.Drawing.Size(500, 0);
            this.grbBodyStyle.Name = "grbBodyStyle";
            this.grbBodyStyle.Padding = new System.Windows.Forms.Padding(2, 4, 2, 12);
            this.grbBodyStyle.Size = new System.Drawing.Size(700, 167);
            this.grbBodyStyle.TabIndex = 0;
            this.grbBodyStyle.TabStop = false;
            this.grbBodyStyle.Text = "Generated member default body style";
            // 
            // tbxSourceCode
            // 
            this.tbxSourceCode.Location = new System.Drawing.Point(201, 104);
            this.tbxSourceCode.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.tbxSourceCode.Name = "tbxSourceCode";
            this.tbxSourceCode.Size = new System.Drawing.Size(282, 26);
            this.tbxSourceCode.TabIndex = 4;
            this.tbxSourceCode.Text = "??";
            // 
            // rdbUncompilable
            // 
            this.rdbUncompilable.AutoSize = true;
            this.rdbUncompilable.Location = new System.Drawing.Point(4, 104);
            this.rdbUncompilable.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.rdbUncompilable.Name = "rdbUncompilable";
            this.rdbUncompilable.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.rdbUncompilable.Size = new System.Drawing.Size(189, 24);
            this.rdbUncompilable.TabIndex = 3;
            this.rdbUncompilable.Text = "Uncompilable code:";
            this.ttpMain.SetToolTip(this.rdbUncompilable, "Use (possibly) uncompilable value as default body");
            this.rdbUncompilable.UseVisualStyleBackColor = true;
            // 
            // rdbDefaultValue
            // 
            this.rdbDefaultValue.AutoSize = true;
            this.tblLayoutPanel.SetColumnSpan(this.rdbDefaultValue, 2);
            this.rdbDefaultValue.Location = new System.Drawing.Point(4, 71);
            this.rdbDefaultValue.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.rdbDefaultValue.Name = "rdbDefaultValue";
            this.rdbDefaultValue.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.rdbDefaultValue.Size = new System.Drawing.Size(193, 24);
            this.rdbDefaultValue.TabIndex = 2;
            this.rdbDefaultValue.Text = "Return default value";
            this.ttpMain.SetToolTip(this.rdbDefaultValue, "Use \'Unchecked.defaultof<_>\' as default body");
            this.rdbDefaultValue.UseVisualStyleBackColor = true;
            // 
            // rdbNotImplementedYet
            // 
            this.rdbNotImplementedYet.AutoSize = true;
            this.tblLayoutPanel.SetColumnSpan(this.rdbNotImplementedYet, 2);
            this.rdbNotImplementedYet.Location = new System.Drawing.Point(4, 38);
            this.rdbNotImplementedYet.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.rdbNotImplementedYet.Name = "rdbNotImplementedYet";
            this.rdbNotImplementedYet.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.rdbNotImplementedYet.Size = new System.Drawing.Size(276, 24);
            this.rdbNotImplementedYet.TabIndex = 1;
            this.rdbNotImplementedYet.Text = "raise NotImplementedException";
            this.ttpMain.SetToolTip(this.rdbNotImplementedYet, "Use \'raise (System.NotImplementedException())\' as default body");
            this.rdbNotImplementedYet.UseVisualStyleBackColor = true;
            // 
            // rdbFailwith
            // 
            this.rdbFailwith.AutoSize = true;
            this.rdbFailwith.Checked = true;
            this.tblLayoutPanel.SetColumnSpan(this.rdbFailwith, 2);
            this.rdbFailwith.Location = new System.Drawing.Point(4, 5);
            this.rdbFailwith.Margin = new System.Windows.Forms.Padding(4, 5, 4, 2);
            this.rdbFailwith.Name = "rdbFailwith";
            this.rdbFailwith.Padding = new System.Windows.Forms.Padding(8, 0, 8, 0);
            this.rdbFailwith.Size = new System.Drawing.Size(259, 24);
            this.rdbFailwith.TabIndex = 0;
            this.rdbFailwith.TabStop = true;
            this.rdbFailwith.Text = "failwith \"Not implemented yet\"";
            this.ttpMain.SetToolTip(this.rdbFailwith, "Use \'failwith \"Not implemented yet\"\' as default body");
            this.rdbFailwith.UseVisualStyleBackColor = true;
            // 
            // tblLayoutPanel
            // 
            this.tblLayoutPanel.AutoSize = true;
            this.tblLayoutPanel.ColumnCount = 2;
            this.tblLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tblLayoutPanel.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tblLayoutPanel.Controls.Add(this.rdbFailwith, 0, 0);
            this.tblLayoutPanel.Controls.Add(this.rdbNotImplementedYet, 0, 1);
            this.tblLayoutPanel.Controls.Add(this.rdbDefaultValue, 0, 2);
            this.tblLayoutPanel.Controls.Add(this.tbxSourceCode, 0, 3);
            this.tblLayoutPanel.Controls.Add(this.rdbUncompilable, 0, 3);
            this.tblLayoutPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.tblLayoutPanel.Location = new System.Drawing.Point(2, 23);
            this.tblLayoutPanel.Margin = new System.Windows.Forms.Padding(0);
            this.tblLayoutPanel.Name = "tblLayoutPanel";
            this.tblLayoutPanel.RowCount = 4;
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 25F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 25F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 25F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 25F));
            this.tblLayoutPanel.Size = new System.Drawing.Size(696, 132);
            this.tblLayoutPanel.TabIndex = 5;
            // 
            // CodeGenerationOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(9F, 20F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grbBodyStyle);
            this.Margin = new System.Windows.Forms.Padding(4, 5, 4, 5);
            this.Name = "CodeGenerationOptionsControl";
            this.Size = new System.Drawing.Size(700, 362);
            this.Load += new System.EventHandler(this.CodeGenerationOptionsControl_Load);
            this.grbBodyStyle.ResumeLayout(false);
            this.grbBodyStyle.PerformLayout();
            this.tblLayoutPanel.ResumeLayout(false);
            this.tblLayoutPanel.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.GroupBox grbBodyStyle;
        private System.Windows.Forms.TextBox tbxSourceCode;
        private System.Windows.Forms.RadioButton rdbUncompilable;
        private System.Windows.Forms.RadioButton rdbDefaultValue;
        private System.Windows.Forms.RadioButton rdbNotImplementedYet;
        private System.Windows.Forms.RadioButton rdbFailwith;
        private System.Windows.Forms.ToolTip ttpMain;
        private System.Windows.Forms.TableLayoutPanel tblLayoutPanel;
    }
}

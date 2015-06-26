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
            this.tblLayoutPanel = new System.Windows.Forms.TableLayoutPanel();
            this.rdbDefaultValue = new System.Windows.Forms.RadioButton();
            this.rdbNotImplementedYet = new System.Windows.Forms.RadioButton();
            this.rdbFailwith = new System.Windows.Forms.RadioButton();
            this.tbxSourceCode = new System.Windows.Forms.TextBox();
            this.rdbUncompilable = new System.Windows.Forms.RadioButton();
            this.ttpMain = new System.Windows.Forms.ToolTip(this.components);
            this.grbOther = new System.Windows.Forms.GroupBox();
            this.tableLayoutOther = new System.Windows.Forms.TableLayoutPanel();
            this.tbxInterfaceMemberIdentifier = new System.Windows.Forms.TextBox();
            this.lblInterfaceMember = new System.Windows.Forms.Label();
            this.grbBodyStyle.SuspendLayout();
            this.tblLayoutPanel.SuspendLayout();
            this.grbOther.SuspendLayout();
            this.tableLayoutOther.SuspendLayout();
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
            this.grbBodyStyle.MinimumSize = new System.Drawing.Size(333, 0);
            this.grbBodyStyle.Name = "grbBodyStyle";
            this.grbBodyStyle.Padding = new System.Windows.Forms.Padding(1, 3, 1, 8);
            this.grbBodyStyle.Size = new System.Drawing.Size(467, 120);
            this.grbBodyStyle.TabIndex = 0;
            this.grbBodyStyle.TabStop = false;
            this.grbBodyStyle.Text = "Generated members\' default body style";
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
            this.tblLayoutPanel.Location = new System.Drawing.Point(1, 16);
            this.tblLayoutPanel.Margin = new System.Windows.Forms.Padding(0);
            this.tblLayoutPanel.Name = "tblLayoutPanel";
            this.tblLayoutPanel.RowCount = 4;
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 25F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 25F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 25F));
            this.tblLayoutPanel.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 25F));
            this.tblLayoutPanel.Size = new System.Drawing.Size(465, 96);
            this.tblLayoutPanel.TabIndex = 5;
            // 
            // rdbDefaultValue
            // 
            this.rdbDefaultValue.AutoSize = true;
            this.tblLayoutPanel.SetColumnSpan(this.rdbDefaultValue, 2);
            this.rdbDefaultValue.Location = new System.Drawing.Point(3, 51);
            this.rdbDefaultValue.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.rdbDefaultValue.Name = "rdbDefaultValue";
            this.rdbDefaultValue.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.rdbDefaultValue.Size = new System.Drawing.Size(131, 17);
            this.rdbDefaultValue.TabIndex = 2;
            this.rdbDefaultValue.Text = "Return default value";
            this.ttpMain.SetToolTip(this.rdbDefaultValue, "Use \'Unchecked.defaultof<_>\' as default body");
            this.rdbDefaultValue.UseVisualStyleBackColor = true;
            // 
            // rdbNotImplementedYet
            // 
            this.rdbNotImplementedYet.AutoSize = true;
            this.tblLayoutPanel.SetColumnSpan(this.rdbNotImplementedYet, 2);
            this.rdbNotImplementedYet.Location = new System.Drawing.Point(3, 27);
            this.rdbNotImplementedYet.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.rdbNotImplementedYet.Name = "rdbNotImplementedYet";
            this.rdbNotImplementedYet.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.rdbNotImplementedYet.Size = new System.Drawing.Size(184, 17);
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
            this.rdbFailwith.Location = new System.Drawing.Point(3, 3);
            this.rdbFailwith.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.rdbFailwith.Name = "rdbFailwith";
            this.rdbFailwith.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.rdbFailwith.Size = new System.Drawing.Size(176, 17);
            this.rdbFailwith.TabIndex = 0;
            this.rdbFailwith.TabStop = true;
            this.rdbFailwith.Text = "failwith \"Not implemented yet\"";
            this.ttpMain.SetToolTip(this.rdbFailwith, "Use \'failwith \"Not implemented yet\"\' as default body");
            this.rdbFailwith.UseVisualStyleBackColor = true;
            // 
            // tbxSourceCode
            // 
            this.tbxSourceCode.Location = new System.Drawing.Point(138, 75);
            this.tbxSourceCode.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.tbxSourceCode.Name = "tbxSourceCode";
            this.tbxSourceCode.Size = new System.Drawing.Size(189, 20);
            this.tbxSourceCode.TabIndex = 4;
            this.tbxSourceCode.Text = "??";
            // 
            // rdbUncompilable
            // 
            this.rdbUncompilable.AutoSize = true;
            this.rdbUncompilable.Location = new System.Drawing.Point(3, 75);
            this.rdbUncompilable.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.rdbUncompilable.Name = "rdbUncompilable";
            this.rdbUncompilable.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.rdbUncompilable.Size = new System.Drawing.Size(129, 17);
            this.rdbUncompilable.TabIndex = 3;
            this.rdbUncompilable.Text = "Uncompilable code:";
            this.ttpMain.SetToolTip(this.rdbUncompilable, "Use (possibly) uncompilable value as default body");
            this.rdbUncompilable.UseVisualStyleBackColor = true;
            // 
            // grbOther
            // 
            this.grbOther.AutoSize = true;
            this.grbOther.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.grbOther.Controls.Add(this.tableLayoutOther);
            this.grbOther.Dock = System.Windows.Forms.DockStyle.Top;
            this.grbOther.Location = new System.Drawing.Point(0, 120);
            this.grbOther.Margin = new System.Windows.Forms.Padding(0);
            this.grbOther.MinimumSize = new System.Drawing.Size(333, 0);
            this.grbOther.Name = "grbOther";
            this.grbOther.Padding = new System.Windows.Forms.Padding(1, 3, 1, 8);
            this.grbOther.Size = new System.Drawing.Size(467, 48);
            this.grbOther.TabIndex = 1;
            this.grbOther.TabStop = false;
            this.grbOther.Text = "Other options";
            // 
            // tableLayoutOther
            // 
            this.tableLayoutOther.AutoSize = true;
            this.tableLayoutOther.ColumnCount = 2;
            this.tableLayoutOther.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutOther.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutOther.Controls.Add(this.lblInterfaceMember, 0, 0);
            this.tableLayoutOther.Controls.Add(this.tbxInterfaceMemberIdentifier, 1, 0);
            this.tableLayoutOther.Dock = System.Windows.Forms.DockStyle.Top;
            this.tableLayoutOther.Location = new System.Drawing.Point(1, 16);
            this.tableLayoutOther.Margin = new System.Windows.Forms.Padding(0);
            this.tableLayoutOther.Name = "tableLayoutOther";
            this.tableLayoutOther.RowCount = 1;
            this.tableLayoutOther.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.tableLayoutOther.Size = new System.Drawing.Size(465, 24);
            this.tableLayoutOther.TabIndex = 5;
            // 
            // tbxInterfaceMemberIdentifier
            // 
            this.tbxInterfaceMemberIdentifier.Location = new System.Drawing.Point(213, 3);
            this.tbxInterfaceMemberIdentifier.Margin = new System.Windows.Forms.Padding(3, 3, 3, 1);
            this.tbxInterfaceMemberIdentifier.Name = "tbxInterfaceMemberIdentifier";
            this.tbxInterfaceMemberIdentifier.Size = new System.Drawing.Size(189, 20);
            this.tbxInterfaceMemberIdentifier.TabIndex = 4;
            this.tbxInterfaceMemberIdentifier.Text = "x";
            this.ttpMain.SetToolTip(this.tbxInterfaceMemberIdentifier, "Declare object identifiers e.g. \'x\', \'this\', \'__\', etc.");
            // 
            // lblInterfaceMember
            // 
            this.lblInterfaceMember.Anchor = System.Windows.Forms.AnchorStyles.Left;
            this.lblInterfaceMember.AutoSize = true;
            this.lblInterfaceMember.Location = new System.Drawing.Point(3, 5);
            this.lblInterfaceMember.Name = "lblInterfaceMember";
            this.lblInterfaceMember.Size = new System.Drawing.Size(204, 13);
            this.lblInterfaceMember.TabIndex = 6;
            this.lblInterfaceMember.Text = "Identifier for implement interface members:";
            this.lblInterfaceMember.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // CodeGenerationOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grbOther);
            this.Controls.Add(this.grbBodyStyle);
            this.Name = "CodeGenerationOptionsControl";
            this.Size = new System.Drawing.Size(467, 235);
            this.Load += new System.EventHandler(this.CodeGenerationOptionsControl_Load);
            this.grbBodyStyle.ResumeLayout(false);
            this.grbBodyStyle.PerformLayout();
            this.tblLayoutPanel.ResumeLayout(false);
            this.tblLayoutPanel.PerformLayout();
            this.grbOther.ResumeLayout(false);
            this.grbOther.PerformLayout();
            this.tableLayoutOther.ResumeLayout(false);
            this.tableLayoutOther.PerformLayout();
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
        private System.Windows.Forms.GroupBox grbOther;
        private System.Windows.Forms.TableLayoutPanel tableLayoutOther;
        private System.Windows.Forms.Label lblInterfaceMember;
        private System.Windows.Forms.TextBox tbxInterfaceMemberIdentifier;
    }
}

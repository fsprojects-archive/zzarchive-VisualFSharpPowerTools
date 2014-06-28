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
            this.grbBodyStyle.SuspendLayout();
            this.SuspendLayout();
            // 
            // grbBodyStyle
            // 
            this.grbBodyStyle.Controls.Add(this.tbxSourceCode);
            this.grbBodyStyle.Controls.Add(this.rdbUncompilable);
            this.grbBodyStyle.Controls.Add(this.rdbDefaultValue);
            this.grbBodyStyle.Controls.Add(this.rdbNotImplementedYet);
            this.grbBodyStyle.Controls.Add(this.rdbFailwith);
            this.grbBodyStyle.Location = new System.Drawing.Point(15, 3);
            this.grbBodyStyle.Name = "grbBodyStyle";
            this.grbBodyStyle.Size = new System.Drawing.Size(368, 204);
            this.grbBodyStyle.TabIndex = 0;
            this.grbBodyStyle.TabStop = false;
            this.grbBodyStyle.Text = "Generated member default body style";
            // 
            // tbxSourceCode
            // 
            this.tbxSourceCode.Location = new System.Drawing.Point(148, 89);
            this.tbxSourceCode.Name = "tbxSourceCode";
            this.tbxSourceCode.Size = new System.Drawing.Size(189, 20);
            this.tbxSourceCode.TabIndex = 4;
            this.tbxSourceCode.Text = "??";
            // 
            // rdbUncompilable
            // 
            this.rdbUncompilable.AutoSize = true;
            this.rdbUncompilable.Location = new System.Drawing.Point(29, 89);
            this.rdbUncompilable.Name = "rdbUncompilable";
            this.rdbUncompilable.Size = new System.Drawing.Size(119, 17);
            this.rdbUncompilable.TabIndex = 3;
            this.rdbUncompilable.Text = "Uncompilable code:";
            this.ttpMain.SetToolTip(this.rdbUncompilable, "Use (possibly) uncompilable value as default body");
            this.rdbUncompilable.UseVisualStyleBackColor = true;
            // 
            // rdbDefaultValue
            // 
            this.rdbDefaultValue.AutoSize = true;
            this.rdbDefaultValue.Location = new System.Drawing.Point(29, 66);
            this.rdbDefaultValue.Name = "rdbDefaultValue";
            this.rdbDefaultValue.Size = new System.Drawing.Size(121, 17);
            this.rdbDefaultValue.TabIndex = 2;
            this.rdbDefaultValue.Text = "Return default value";
            this.ttpMain.SetToolTip(this.rdbDefaultValue, "Use \'Unchecked.defaultof<_>\' as default body");
            this.rdbDefaultValue.UseVisualStyleBackColor = true;
            // 
            // rdbNotImplementedYet
            // 
            this.rdbNotImplementedYet.AutoSize = true;
            this.rdbNotImplementedYet.Location = new System.Drawing.Point(29, 43);
            this.rdbNotImplementedYet.Name = "rdbNotImplementedYet";
            this.rdbNotImplementedYet.Size = new System.Drawing.Size(174, 17);
            this.rdbNotImplementedYet.TabIndex = 1;
            this.rdbNotImplementedYet.Text = "raise NotImplementedException";
            this.ttpMain.SetToolTip(this.rdbNotImplementedYet, "Use \'raise (System.NotImplementedException())\' as default body");
            this.rdbNotImplementedYet.UseVisualStyleBackColor = true;
            // 
            // rdbFailwith
            // 
            this.rdbFailwith.AutoSize = true;
            this.rdbFailwith.Checked = true;
            this.rdbFailwith.Location = new System.Drawing.Point(29, 20);
            this.rdbFailwith.Name = "rdbFailwith";
            this.rdbFailwith.Size = new System.Drawing.Size(166, 17);
            this.rdbFailwith.TabIndex = 0;
            this.rdbFailwith.TabStop = true;
            this.rdbFailwith.Text = "failwith \"Not implemented yet\"";
            this.ttpMain.SetToolTip(this.rdbFailwith, "Use \'failwith \"Not implemented yet\"\' as default body");
            this.rdbFailwith.UseVisualStyleBackColor = true;
            // 
            // CodeGenerationOptionsControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.grbBodyStyle);
            this.Name = "CodeGenerationOptionsControl";
            this.Size = new System.Drawing.Size(467, 235);
            this.Load += new System.EventHandler(this.CodeGenerationOptionsControl_Load);
            this.grbBodyStyle.ResumeLayout(false);
            this.grbBodyStyle.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox grbBodyStyle;
        private System.Windows.Forms.TextBox tbxSourceCode;
        private System.Windows.Forms.RadioButton rdbUncompilable;
        private System.Windows.Forms.RadioButton rdbDefaultValue;
        private System.Windows.Forms.RadioButton rdbNotImplementedYet;
        private System.Windows.Forms.RadioButton rdbFailwith;
        private System.Windows.Forms.ToolTip ttpMain;
    }
}

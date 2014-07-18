using EnvDTE;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FSharpVSPowerTools
{
    public class Utils
    {
        public static string GetDefaultMemberBody(IServiceProvider serviceProvider)
        {
            var env = (DTE)serviceProvider.GetService(typeof(DTE));
            var props = env.get_Properties(Resource.vsPackageTitle, "Code Generation");
            var codeGenOptions = (CodeGenerationOptions)props.Item("CodeGenerationOptions").Value;

            switch (codeGenOptions)
            {
                case CodeGenerationOptions.Failwith:
                    return "failwith \"Not implemented yet\"";
                case CodeGenerationOptions.NotImplementedYet:
                    return "raise (System.NotImplementedException())";
                case CodeGenerationOptions.DefaultValue:
                    return "Unchecked.defaultof<_>";
                default:
                    var defaultBody = (string)props.Item("DefaultBody").Value;
                    return defaultBody;
            }
        }

        public static IGeneralOptionsPage GetGeneralOptionsPage(IServiceProvider serviceProvider)
        {
            return serviceProvider.GetService(typeof(GeneralOptionsPage)) as IGeneralOptionsPage;
        }
    }
}

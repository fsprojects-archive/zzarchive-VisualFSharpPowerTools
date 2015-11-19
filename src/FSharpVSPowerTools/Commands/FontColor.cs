using System.Windows.Media;

namespace FSharpVSPowerTools
{
    public class FontColor
    {
        public readonly Color? Foreground;
        public readonly Color? Background;

        public FontColor(Color? foreground = null, Color? background = null)
        {
            Foreground = foreground;
            Background = background;
        }
    }
}
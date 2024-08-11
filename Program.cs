using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace RleSearch
{
    class HistoryPattern : IEquatable<HistoryPattern>
    {
        public readonly int Width;
        public readonly int Height;
        byte[] cells;

        public HistoryPattern(int width, int height)
        {
            Width = width;
            Height = height;
            cells = new byte[Width * Height];
        }

        public byte this[int x, int y]
        {
            get
            {
                return cells[x + y * Width];
            }
            set
            {
                cells[x + y * Width] = value;
            }
        }

        public static HistoryPattern ReadRLE(params string[] lines)
        {
            int x = 0;
            int y = 0;

            string scount = "";
            HistoryPattern pattern = null;
            foreach (var line in lines)
            {
                if (line.StartsWith("#"))
                    continue;
                if (line.StartsWith("x"))
                {
                    var match = Regex.Match(line, "^x = ([0-9]+), y = ([0-9]+)");
                    if (!match.Success)
                        throw new Exception();
                    int width = int.Parse(match.Groups[1].Value);
                    int height = int.Parse(match.Groups[2].Value);
                    pattern = new HistoryPattern(width, height);
                    continue;
                }

                for (int i = 0; i < line.Length; i++)
                {
                    char c = line[i];
                    if (c >= '0' && c <= '9')
                    {
                        scount += c;
                    }
                    else
                    {
                        if (c == '$')
                        {
                            x = 0;
                            int count = 1;
                            if (scount != "")
                                count = int.Parse(scount);
                            y += count;
                            scount = "";
                        }
                        else if (c == '!')
                            break;
                        else if (c == '.' || (c >= 'A' && c <= 'Z'))
                        {
                            int count = 1;
                            if (scount != "")
                                count = int.Parse(scount);
                            for (int k = 0; k < count; k++)
                            {
                                pattern[x, y] = c == '.' ? (byte)0 : (byte)(c - 'A' + 1);
                                x++;
                                pattern.WrapCoordinates(ref x, ref y);
                            }
                            scount = "";
                        }
                    }
                }
            }
            return pattern;
        }

        public static HistoryPattern ReadRLE(string fileName)
        {
            return ReadRLE(File.ReadAllLines(fileName));
        }

        public override int GetHashCode()
        {
            int hash = Width << 16 | Height;
            int shift = 0;
            for (int i = 0; i < cells.Length; i++)
            {
                hash = hash ^ cells[i] << shift;
                shift = (shift + 1) & 31;
            }
            return hash;
        }

        public bool Equals(HistoryPattern other)
        {
            if (Width != other.Width || Height != other.Height)
                return false;
            for (int i = 0; i < cells.Length; i++)
                if (cells[i] != other.cells[i])
                    return false;
            return true;
        }

        void WrapCoordinates(ref int x, ref int y)
        {
            if (x < 0)
                x += Width;
            else if (x >= Width)
                x -= Width;
            if (y < 0)
                y += Height;
            else if (y >= Height)
                y -= Height;
        }
    }

    class Pattern : IEquatable<Pattern>
    {
        public readonly int Width;
        public readonly int Height;

        public readonly int WordWidth;
        public readonly int WordCount;
        ulong[] cells;

        public Pattern(int width, int height)
        {
            Width = width;
            Height = height;
            WordWidth = (Width + 63) / 64;
            WordCount = WordWidth * Height;
            cells = new ulong[WordCount];
        }

        public Pattern(int width, int height, Pattern source, int offsetX, int offsetY)
            : this(width, height)
        {
            for (int y = 0; y < Height; y++)
                for (int x = 0; x < Width; x++)
                    this[x, y] = source[x + offsetX, y + offsetY];
        }

        public bool this[int x, int y]
        {
            get
            {
                int div = x / 64;
                int shift = x % 64;
                return (cells[div + y * WordWidth] >> shift & 1UL) == 1UL;
            }
            set
            {
                int div = x / 64;
                int shift = x % 64;
                cells[div + y * WordWidth] &= ~(1UL << shift);
                cells[div + y * WordWidth] |= (value ? 1UL : 0UL) << shift;
            }
        }

        private static void HalfAdder(ulong a, ulong b, out ulong carry, out ulong sum)
        {
            sum = a ^ b;
            carry = a & b;
        }

        private static void FullAdder(ulong a, ulong b, ulong c, out ulong carry, out ulong sum)
        {
            ulong temp = a ^ b;
            sum = temp ^ c;
            carry = a & b | temp & c;
        }

        public Pattern Advance()
        {
            ulong[] n1 = new ulong[WordCount];
            ulong[] n2 = new ulong[WordCount];
            for (int y = 0; y < Height; y++)
            {
                for (int x = 0; x < WordWidth; x++)
                {
                    ulong w = cells[x + y * WordWidth];
                    n1[x + y * WordWidth] |= w >> 1;
                    n2[x + y * WordWidth] |= w << 1;
                    if (x != 0)
                        n1[x - 1 + y * WordWidth] |= w << 63;
                    else
                    {
                        int shift = (Width + 63) % 64;
                        n1[WordWidth - 1 + y * WordWidth] |= (w & 1UL) << shift;
                    }
                    if (x != WordWidth - 1)
                        n2[x + 1 + y * WordWidth] |= w >> 63;
                    else
                    {
                        int shift1 = 64 - Width % 64;
                        ulong mask = 0xFFFFFFFFFFFFFFFFUL >> shift1;
                        n2[x + y * WordWidth] &= mask;
                        int shift2 = (Width - 1) % 64;
                        n2[y * WordWidth] |= w >> shift2;
                    }
                }
            }

            var result = new Pattern(Width, Height);
            for (int i = 0; i < cells.Length; i++)
            {
                int im1 = i - WordWidth < 0 ? i - WordWidth + WordCount : i - WordWidth;
                int ip1 = i + WordWidth >= WordCount ? i + WordWidth - WordCount : i + WordWidth;
                ulong j, k, l, m, n, o;
                FullAdder(n1[im1], n1[i], n1[ip1], out m, out j);
                FullAdder(n2[im1], n2[i], n2[ip1], out n, out k);
                HalfAdder(cells[im1], cells[ip1], out o, out l);
                ulong x, y, z, w;
                FullAdder(j, k, l, out y, out w);
                FullAdder(m, n, o, out x, out z);
                result.cells[i] = (cells[i] | w) & (y ^ z) & ~x;
            }
            return result;
        }

        public void Clear(int startX, int startY, int countX, int countY)
        {
            for (int y = 0; y < countY; y++)
                for (int x = 0; x < countX; x++)
                    this[x + startX, y + startY] = false;
        }

        public bool Search(Pattern pattern, int startX, int startY, int countX, int countY)
        {
            for (int y1 = 0; y1 < countY; y1++)
            {
                for (int x1 = 0; x1 < countX; x1++)
                {
                    bool found = true;
                    for (int y2 = 0; y2 < pattern.Height; y2++)
                    {
                        for (int x2 = 0; x2 < pattern.Width; x2++)
                        {
                            if (this[startX + x1 + x2, startY + y1 + y2] != pattern[x2, y2])
                            {
                                found = false;
                                break;
                            }
                        }
                        if (!found)
                            break;
                    }
                    if (found)
                        return true;
                }
            }
            return false;
        }

        public void Stamp(Pattern stamp, int xo, int yo)
        {
            for (int y = 0; y < stamp.Height; y++)
                for (int x = 0; x < stamp.Width; x++)
                    this[x + xo, y + yo] |= stamp[x, y];
        }

        public void WriteRLE(string fileName, string comment = null)
        {
            var sb = new StringBuilder();
            if (comment != null)
                sb.AppendLine($"#C {comment}");
            sb.AppendLine($"x = {Width}, y = {Height}, rule = B3/S23");
            var tags = new List<char>();
            for (int y = 0; y < Height; y++)
            {
                for (int x = 0; x < Width; x++)
                    tags.Add(this[x, y] ? 'o' : 'b');
                tags.Add(y == Height - 1 ? '!' : '$');
            }
            int lineLength = 0;
            for (int i = 0; i < tags.Count; i++)
            {
                int runCount = 1;
                while (i + 1 < tags.Count && tags[i] == tags[i + 1])
                {
                    i++;
                    runCount++;
                }
                string run = runCount == 1 ? $"{tags[i]}" : $"{runCount}{tags[i]}";
                if (lineLength + run.Length > 70)
                {
                    sb.AppendLine();
                    lineLength = 0;
                }
                sb.Append(run);
                lineLength += run.Length;
            }
            File.WriteAllText(fileName, sb.ToString());
        }

        public static Pattern ReadRLE(params string[] lines)
        {
            int x = 0;
            int y = 0;

            string scount = "";
            Pattern pattern = null;
            foreach (var line in lines)
            {
                if (line.StartsWith("#"))
                    continue;
                if (line.StartsWith("x"))
                {
                    var match = Regex.Match(line, "^x = ([0-9]+), y = ([0-9]+)");
                    if (!match.Success)
                        throw new Exception();
                    int width = int.Parse(match.Groups[1].Value);
                    int height = int.Parse(match.Groups[2].Value);
                    pattern = new Pattern(width, height);
                    continue;
                }

                for (int i = 0; i < line.Length; i++)
                {
                    char c = line[i];
                    if (c >= '0' && c <= '9')
                    {
                        scount += c;
                    }
                    else
                    {
                        if (c == '$')
                        {
                            x = 0;
                            int count = 1;
                            if (scount != "")
                                count = int.Parse(scount);
                            y += count;
                            scount = "";
                        }
                        else if (c == '!')
                            break;
                        else if (c == 'o' || c == 'b')
                        {
                            int count = 1;
                            if (scount != "")
                                count = int.Parse(scount);
                            for (int k = 0; k < count; k++)
                            {
                                pattern[x, y] = c == 'o';
                                x++;
                                pattern.WrapCoordinates(ref x, ref y);
                            }
                            scount = "";
                        }
                    }
                }
            }
            return pattern;
        }

        public void ReadRLE(string fileName)
        {
            ReadRLE(File.ReadAllLines(fileName));
        }

        public override int GetHashCode()
        {
            ulong hash = (ulong)(Width << 16 | Height);
            for (int i = 0; i < cells.Length; i++)
                hash = hash ^ cells[i];
            return (int)(hash ^ hash >> 32);
        }

        public bool Equals(Pattern other)
        {
            if (Width != other.Width || Height != other.Height)
                return false;
            for (int i = 0; i < cells.Length; i++)
                if (cells[i] != other.cells[i])
                    return false;
            return true;
        }

        void WrapCoordinates(ref int x, ref int y)
        {
            if (x < 0)
                x += Width;
            else if (x >= Width)
                x -= Width;
            if (y < 0)
                y += Height;
            else if (y >= Height)
                y -= Height;
        }
    }

    class Program
    {
        Pattern gliderTop1;
        Pattern gliderTop2;
        Pattern gliderBottom1;
        Pattern gliderBottom2;
        Pattern gliderLeft1;
        Pattern gliderLeft2;
        Pattern gliderRight1;
        Pattern gliderRight2;

        Program()
        {
            gliderTop1 = Pattern.ReadRLE("x = 3, y = 3", "3o$2bo$bo!");
            gliderTop2 = Pattern.ReadRLE("x = 3, y = 3", "3o$o$bo!");
            gliderBottom1 = Pattern.ReadRLE("x = 3, y = 3", "bo$2bo$3o!");
            gliderBottom2 = Pattern.ReadRLE("x = 3, y = 3", "bo$o$3o!");
            gliderLeft1 = Pattern.ReadRLE("x = 3, y = 3", "2o$obo$o!");
            gliderLeft2 = Pattern.ReadRLE("x = 3, y = 3", "o$obo$2o!");
            gliderRight1 = Pattern.ReadRLE("x = 3, y = 3", "2bo$obo$b2o!");
            gliderRight2 = Pattern.ReadRLE("x = 3, y = 3", "b2o$obo$2bo!");
        }

        Pattern HistoryToLife(HistoryPattern source)
        {
            Pattern result = new Pattern(source.Width, source.Height);
            for (int y = 0; y < source.Height; y++)
                for (int x = 0; x < source.Width; x++)
                    result[x, y] = source[x, y] == 1;
            return result;
        }

        bool Search(HistoryPattern obj, Pattern expanded, ref int objX, ref int objY)
        {
            int countX = expanded.Width - obj.Width;
            int countY = expanded.Height - obj.Height;
            if (countX < 1 || countY < 1)
                return false;

            for (int y1 = 0; y1 < countY; y1++)
            {
                for (int x1 = 0; x1 < countX; x1++)
                {
                    bool found = true;
                    for (int y2 = 0; y2 < obj.Height; y2++)
                    {
                        for (int x2 = 0; x2 < obj.Width; x2++)
                        {
                            bool c1 = expanded[x1 + x2, y1 + y2];
                            int c2 = obj[x2, y2];
                            if (c1 && c2 == 0)
                            {
                                found = false;
                                break;
                            }
                            if (!c1 && (c2 == 1 || c2 == 5))
                            {
                                found = false;
                                break;
                            }
                        }
                        if (!found)
                            break;
                    }
                    if (found)
                    {
                        objX = x1;
                        objY = y1;
                        return true;
                    }
                }
            }
            return false;
        }

        bool Search(HistoryPattern obj, HistoryPattern pattern,
            int ticks, ref int objX, ref int objY, ref int objXperc, ref int objYperc,
            ref int objTick, ref int objOrientation)
        {
            Pattern expanded = new Pattern(pattern.Width + 6, pattern.Height + 6);
            expanded.Stamp(HistoryToLife(pattern), 3, 3);

            var transforms = new Dictionary<HistoryPattern, int>();
            for (int swapxy = 0; swapxy < 2; swapxy++)
                for (int reverseX = 0; reverseX < 2; reverseX++)
                    for (int reverseY = 0; reverseY < 2; reverseY++)
                    {
                        int newWidth = swapxy == 0 ? obj.Width : obj.Height;
                        int newHeight = swapxy == 0 ? obj.Height : obj.Width;
                        var transformed = new HistoryPattern(newWidth, newHeight);
                        for (int y = 0; y < obj.Height; y++)
                        {
                            for (int x = 0; x < obj.Width; x++)
                            {
                                transformed[swapxy == 0 ? x : y, swapxy == 0 ? y : x] =
                                    obj[reverseX == 0 ? x : obj.Width - x - 1,
                                        reverseY == 0 ? y : obj.Height - y - 1];
                            }
                        }
                        int orientation = swapxy * 4 + reverseX * 2 + reverseY;
                        if (!transforms.ContainsKey(transformed))
                            transforms.Add(transformed, orientation);
                    }

            for (int tick = 0; tick < ticks; tick++)
            {
                foreach (var tkv in transforms)
                {
                    if (Search(tkv.Key, expanded, ref objX, ref objY))
                    {
                        objX -= 3;
                        objY -= 3;
                        objXperc = 100 * objX / (pattern.Width - tkv.Key.Width);
                        objYperc = 100 * objY / (pattern.Height - tkv.Key.Height);
                        objTick = tick;
                        objOrientation = tkv.Value;
                        return true;
                    }
                }

                bool gliderFound =
                    expanded.Search(gliderTop1, 0, 0, pattern.Width + 4, 1) ||
                    expanded.Search(gliderTop2, 0, 0, pattern.Width + 4, 1) ||
                    expanded.Search(gliderBottom1, 0, pattern.Height + 3, pattern.Width + 4, 1) ||
                    expanded.Search(gliderBottom2, 0, pattern.Height + 3, pattern.Width + 4, 1) ||
                    expanded.Search(gliderLeft1, 0, 0, 1, pattern.Height + 4) ||
                    expanded.Search(gliderLeft2, 0, 0, 1, pattern.Height + 4) ||
                    expanded.Search(gliderRight1, pattern.Width + 3, 0, 1, pattern.Height + 4) ||
                    expanded.Search(gliderRight2, pattern.Width + 3, 0, 1, pattern.Height + 4);
                if (gliderFound)
                {
                    expanded.Clear(0, 0, expanded.Width, 3);
                    expanded.Clear(0, pattern.Height + 3, expanded.Width, 3);
                    expanded.Clear(0, 0, 3, expanded.Height);
                    expanded.Clear(pattern.Width + 3, 0, 3, expanded.Height);
                }
                expanded = expanded.Advance();
            }
            return false;
        }

        void Run(string objectPath, string searchDirectory, int ticks)
        {
            var obj = HistoryPattern.ReadRLE(objectPath);

            var fileNames = Directory.EnumerateFiles(searchDirectory, "*.rle").ToArray();
            Console.Write($"Loading {fileNames.Length} patterns...");
            var patterns = fileNames.Select(x => HistoryPattern.ReadRLE(x)).ToArray();
            Console.WriteLine(" Done");

            int foundCount = 0;
            for (int i = 0; i < patterns.Length; i++)
            {
                int objX = -1;
                int objXperc = -1;
                int objY = -1;
                int objYperc = -1;
                int objTick = -1;
                int objOrientation = -1;
                var pattern = patterns[i];
                if (Search(obj, pattern, ticks, ref objX, ref objY,
                    ref objXperc, ref objYperc, ref objTick, ref objOrientation))
                {
                    foundCount++;
                    string name = Path.GetFileNameWithoutExtension(fileNames[i]);
                    Console.WriteLine($"{name, -9}| x ={objX, 4} ! {objXperc,3}% | y ={objY, 4} ! {objYperc,3}% | t ={objTick, 3} | o = {objOrientation}");
                }
            }
            Console.WriteLine($"{foundCount} patterns found");
        }

        static void Main(string[] args)
        {
            System.Threading.Thread.CurrentThread.CurrentCulture =
                System.Globalization.CultureInfo.InvariantCulture;

            if (args.Length == 0 || args.Length % 2 != 0)
            {
                Console.WriteLine("Usage: RleSearch -o object.rle -d directory -t ticks");
                return;
            }

            string objectPath = "quadri_snark.rle";
            string searchDirectory = ".";
            int ticks = 64;

            int paramCount = args.Length / 2;
            for (int i = 0; i < paramCount; i++)
            {
                string key = args[i * 2 + 0];
                string val = args[i * 2 + 1];
                if (key == "-o")
                    objectPath = val;
                else if (key == "-d")
                    searchDirectory = val;
                else if (key == "-t")
                {
                    ticks = int.Parse(val);
                    if (ticks < 1)
                        throw new Exception();
                }
                else
                    throw new Exception();
            }

            new Program().Run(objectPath, searchDirectory, ticks);
        }
    }
}

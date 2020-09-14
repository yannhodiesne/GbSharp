namespace GbSharp.Core.CPU
{
    struct Registers
    {
        internal byte A;
        internal byte B;
        internal byte C;
        internal byte D;
        internal byte E;
        internal FlagsRegister F;
        internal byte H;
        internal byte L;

        internal ushort AF
        {
            get => (ushort) (A << 8 | F);
            set
            {
                A = (byte)((value & 0xFF00) >> 8);
                F = (FlagsRegister)(byte)(value & 0xFF);
            }
        }

        internal ushort BC
        {
            get => (ushort)(B << 8 | C);
            set
            {
                B = (byte)((value & 0xFF00) >> 8);
                C = (byte)(value & 0xFF);
            }
        }

        internal ushort DE
        {
            get => (ushort)(D << 8 | E);
            set
            {
                D = (byte)((value & 0xFF00) >> 8);
                E = (byte)(value & 0xFF);
            }
        }

        internal ushort HL
        {
            get => (ushort)(H << 8 | L);
            set
            {
                H = (byte)((value & 0xFF00) >> 8);
                L = (byte)(value & 0xFF);
            }
        }
    }
}

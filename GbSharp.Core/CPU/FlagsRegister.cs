namespace GbSharp.Core.CPU
{
    struct FlagsRegister
    {
        internal bool Zero { get; set; }
        internal bool Subtract { get; set; }
        internal bool HalfCarry { get; set; }
        internal bool Carry { get; set; }

        private const byte ZeroFlagBytePosition = 7;
        private const byte SubtractFlagBytePosition = 6;
        private const byte HalfCarryFlagBytePosition = 5;
        private const byte CarryFlagBytePosition = 4;

        public static implicit operator byte(FlagsRegister flag) => (byte)((flag.Zero ? 1 : 0) << ZeroFlagBytePosition |
                                                                           (flag.Subtract ? 1 : 0) << SubtractFlagBytePosition |
                                                                           (flag.HalfCarry ? 1 : 0) << HalfCarryFlagBytePosition |
                                                                           (flag.Carry ? 1 : 0) << CarryFlagBytePosition
        );

        public static explicit operator FlagsRegister(byte value) => new FlagsRegister
        {
            Zero = ((value >> ZeroFlagBytePosition) & 0b1) != 0,
            Subtract = ((value >> SubtractFlagBytePosition) & 0b1) != 0,
            HalfCarry = ((value >> HalfCarryFlagBytePosition) & 0b1) != 0,
            Carry = ((value >> CarryFlagBytePosition) & 0b1) != 0,
        };
    }
}

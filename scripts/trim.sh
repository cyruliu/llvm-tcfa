while getopts "d:" arg
do
  case $arg in
    d)
      bindir=$OPTARG
      ;;
  esac
done

for file in `ls $bindir/*.c`; do
	echo $file
	sed -i "/__cplusplus/,+29d" $file

	sed -i "s/_uint32_t/_ureplace_u32int/g" $file
	sed -i "s/_uint64_t/_ureplace_u64int/g" $file
	sed -i "s/_uint16_t/_ureplace_u16int/g" $file
	sed -i "s/_uint8_t/_ureplace_u8int/g" $file

	sed -i "s/uint32_t/unsigned int/g" $file
	sed -i "s/int32_t/int/g" $file
	sed -i "s/uint64_t/unsigned long/g" $file
	sed -i "s/int64_t/long/g" $file
	sed -i "s/uint8_t/unsigned char/g" $file
	sed -i "s/int8_t/char/g" $file
	sed -i "s/uint16_t/unsigned short/g" $file
	sed -i "s/int16_t/short/g" $file
	sed -i "s/bool/char/g" $file

	sed -i "/ifdef _MSC_VER/,+2d" $file
	sed -i "/__ATTRIBUTELIST__/d" $file
	sed -i "s/__ATTRIBUTE_WEAK__//g" $file
	sed -i "s/__forceinline//g" $file
	sed -i "s/(uintptr_t)//g" $file
	sed -i "s/UINT64_C([[:digit:]]*/&UL/g" $file
	sed -i "s/UINT64_C//g" $file
done

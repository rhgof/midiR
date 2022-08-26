# Midi Byte Level Operations

# These numbers are represented 7 bits per byte, most significant bits first.
# All bytes except the last have bit 7 set, and the last byte has bit 7 clear.
# If the number is between 0 and 127, it is thus represented exactly as one byte.

# bytesVarLenIntOld <- function(i) {
#   inputbytes <- packBits(rev(intToBits(i)),type="raw")
#
#   N = length(inputbytes)
#
#   # skip leading 0x00's
#   for (n in 1:N) {
#     if (inputbytes[n] != 0x00) break
#   }
#
#   inputbytes <- inputbytes[n:N]
#   N = length(inputbytes)
#
#   bytes = raw(N)
#   for (b in 1:N) {
#     bytes[b] = packBits(rev(rawToBits(inputbytes[b])))
#     # set bit 7 on all except last
#     if (b < N) bytes[b] = bytes[b] | as.raw(0x80)
#   }
#
#   return(bytes)
# }

bytesVarLenInt <- function(value) {
  #value = 511
  bytes = raw(4)
  bits = intToBits(value)

  bytes[4] = packBits(c(bits[1:7],as.raw(0x00)) )
  bytes[3] = packBits(c(bits[8:14],as.raw(0x00)) )
  bytes[2] = packBits(c(bits[15:21],as.raw(0x00)) )
  bytes[1] = packBits(c(bits[22:28],as.raw(0x00)) )

  N = length(bytes)

  # skip leading 0x00's
  for (n in 1:N) {
    if (bytes[n] != 0x00) break
  }

  outbytes <- bytes[n:N]
  N = length(outbytes)

  for (b in 1:N) {
    if (b < N) outbytes[b] = outbytes[b] | as.raw(0x80)
  }

  return(outbytes)
}


# 4-byte 32-bit integers

bytesInt <- function(i) {
  bits = rev(intToBits(i))
  bytes <- packBits(rev(intToBits(i)),type="raw")

  N = length(bytes)
  for (n in 1:N) {
    bytes[n] <- packBits(rev(rawToBits(bytes[n])))
  }
  return(bytes)
}

# 2byte 16-bit unsigned integers
bytesShort <- function(i) {
  bytes = raw(2)
  bits = rev(intToBits(i))
  longbytes <- packBits(rev(intToBits(i)),type="raw")

  N = length(bytes)
  for (n in 1:2) {
    bytes[n] <- packBits(rev(rawToBits(longbytes[n+2])))
  }
  #writeBin(bytes,con, endian = "little")
  return(bytes)
}

# char string to bytes

bytesChar <- function(s) {
  bytes = charToRaw(s)
  return(bytes)
}

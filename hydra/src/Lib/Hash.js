import Blake2 from "blakejs";

export const blake2b256Hash = (str) =>
  Blake2.blake2b(str, null, 32);

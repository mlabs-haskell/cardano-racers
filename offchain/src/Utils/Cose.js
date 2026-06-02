import * as lib from "@mlabs-haskell/cardano-message-signing";

// -------------------------------------------------------------------
// COSESign1

export const getCoseSign1Signature = (coseSign1Bytes) => () =>
  lib.COSESign1.from_bytes(coseSign1Bytes).signature();

// -------------------------------------------------------------------
// SigStructure

// newSigStructure :: ByteArray -> ProtectedHeaderMap -> ByteArray
export const newSigStructure = (payload) => (headers) =>
  lib.SigStructure.new(lib.SigContext.Signature1, headers, [], payload).to_bytes();

// -------------------------------------------------------------------
// ProtectedHeaderMap

// newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap
export const newProtectedHeaderMap = (headerMap) => lib.ProtectedHeaderMap.new(headerMap);

// -------------------------------------------------------------------
// HeaderMap

// newHeaderMap :: Effect HeaderMap
export const newHeaderMap = () => lib.HeaderMap.new();

// setAlgHeaderToEdDsa :: HeaderMap -> Effect Unit
export const setAlgHeaderToEdDsa = (headerMap) => () => {
  const label = lib.Label.from_algorithm_id(lib.AlgorithmId.EdDSA);
  headerMap.set_algorithm_id(label);
};

// setAddressHeader :: ByteArray -> HeaderMap -> Effect Unit
export const setAddressHeader = (addressBytes) => (headerMap) => () => {
  const label = lib.Label.new_text("address");
  const value = lib.CBORValue.new_bytes(addressBytes);
  headerMap.set_header(label, value);
};

// -------------------------------------------------------------------
// CoseKey

// fromBytesCoseKey :: CborBytes -> Effect CoseKey
export const fromBytesCoseKey = (bytes) => () => {
  return lib.COSEKey.from_bytes(bytes);
};

// getCoseKeyHeaderX :: MaybeFfiHelper -> CoseKey -> Maybe ByteArray
export const getCoseKeyHeaderX = (maybe) => (coseKey) => {
  const cborValue = coseKey.header(
    lib.Label.new_int(
      lib.Int.new_negative(lib.BigNum.from_str("2")) // x (-2)
    )
  );
  return opt_chain(maybe, cborValue, "as_bytes");
};

// Helpers

function opt_chain(maybe, obj) {
  const isNothing = (x) => x === null || x === undefined;
  let result = obj;
  for (let i = 2; i < arguments.length; i++) {
    if (isNothing(result)) {
      return maybe.nothing;
    } else {
      result = result[arguments[i]]();
    }
  }
  return isNothing(result) ? maybe.nothing : maybe.just(result);
}

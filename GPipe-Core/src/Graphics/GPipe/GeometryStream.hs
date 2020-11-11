module Graphics.GPipe.GeometryStream (
    -- * The data type
    GeometryStream(),

    -- * Creating GeometryStream
    geometrize,
    generateAndRasterize,

    -- * Various GeometryStream operations
    generativePoints,
    generativeLineStrip,
    generativeTriangleStrip,
    emitVertex,
    emitVertexPosition,
    emitVertexLayer,
    emitVertexPositionAndLayer,
    endPrimitive,
)
where

import Graphics.GPipe.Internal.GeometryStream

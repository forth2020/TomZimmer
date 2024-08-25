GLOBAL_FUNC t_void DecodeRLEBitfields (
    const t_byte        **source_pptr,
    t_pixel_count num_pixels_to_write,
    t_colour            *row_start_ptr,
    t_dimension         block_width,
    t_dimension         frame_width,
    e_colourdepth format,
    t_bool              enhanced_mode)
{
    t_pixel_count     pixel_count;
    t_dimension             column_no;
    int                     bits_per_pixel;

    bits_per_pixel = (int) format;
    column_no = 0;

    for (pixel_count = 0; pixel_count < num_pixels_to_write;)
    {
        if (enhanced_mode)
        {
            /* skip the indicated number of pixels in the block */

            pixel_count += SkipPixels (source_pptr,
                &row_start_ptr,
                &column_no,
                block_width,
                frame_width);
            }

        /* if still more pixels to write, decode the next RLE packet */

        if (pixel_count < num_pixels_to_write)
        {
            t_byte              byte;
            t_byte              packed_pixels;
            t_pixel_count num_bytes_to_process;

            byte = *(*source_pptr)++;

            if ('\0' > (t_char) byte)
            {
                /* it's a literal run */

                num_bytes_to_process = (t_byte) -(t_char) byte;

                /* process each literal byte */

                while (0 < num_bytes_to_process
                    && pixel_count < num_pixels_to_write)
                {
                    packed_pixels = *(*source_pptr)++;
                    num_bytes_to_process--;

                    PlaceBitfields (packed_pixels,
                        bits_per_pixel,
                        &pixel_count,
                        num_pixels_to_write,
                        &row_start_ptr,
                        &column_no,
                        block_width,
                        frame_width);
                    }

/*
* skip remaining literal bytes in packet
* if we overflowed the output region
                */

                if (0 < num_bytes_to_process)
                {
                    *source_pptr += num_bytes_to_process;
                    }
                }
            else
            {
                /* it's a replicate run */

                num_bytes_to_process = byte + 1;

                packed_pixels = *(*source_pptr)++;

                /* process the replica byte over and over */

                while (0 < num_bytes_to_process
                    && pixel_count < num_pixels_to_write)
                {
                    num_bytes_to_process--;

                    PlaceBitfields (packed_pixels,
                        bits_per_pixel,
                        &pixel_count,
                        num_pixels_to_write,
                        &row_start_ptr,
                        &column_no,
                        block_width,
                        frame_width);
                    }
                }
            }
        }
    }
}
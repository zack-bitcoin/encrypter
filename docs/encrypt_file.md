March 1 2018

If you don't want to use jiffy, and you don't care about compatibility with javascript, then you can replace `packer:pack` with `term_to_binary`, and replace `packer:unpack` with `binary_to_term`.

The output of encryption:encrypt is a tuple, so you can use term_to_binary again to be able to store the encrypted data to a file.

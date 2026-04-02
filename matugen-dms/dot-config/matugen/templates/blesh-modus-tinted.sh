ble-import contrib/scheme/base16
ble/contrib/scheme:base16/initialize
ble-face -s auto_complete 'fg=238'

ble-import contrib/colorglass
<* if {{ is_dark_mode }} *>
  bleopt colorglass_base16_palette=ModusVivendiTinted
<* else *>
  bleopt colorglass_base16_palette=ModusOperandiTinted
<* endif *>

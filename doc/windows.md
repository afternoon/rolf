Rolf on Windows
===============

1. Install Erlang R14B02.

    http://www.erlang.org/download.html

2. Download Rolf release package (or generate your own with rebar).

    https://github.com/downloads/afternoon/rolf/rolf-0.1.zip

3. Add Rolf to Windows Services

    c:\erl5.8.3\erts-5.8.3\bin\erlsrv.exe add Rolf -c "Collects system data for monitoring." -w c:\rolf -m c:\erl5.8.3\erts-5.8.3\bin\start_erl.exe -debugtype reuse -args "-setcookie rolf123 -boot c:\rolf\release\0.1\rolf.boot -embedded -config c:\rolf\etc\app.config -args_file c:\rolf\etc\vm.args ++ -reldir c:\rolf\releases"
    c:\erl5.8.3\erts-5.8.3\bin\erlsrv.exe add Rolf -c "Collects system data for monitoring." -w c:\rolf -m c:\erl5.8.3\erts-5.8.3\bin\start_erl.exe -debugtype reuse -args "-setcookie rolf123 ++ -reldir c:\rolf\releases"

4. Start the service!

    c:\erl5.8.3\erts-5.8.3\bin\erlsrv.exe start Rolf

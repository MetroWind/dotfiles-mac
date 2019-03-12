report-overwritten()
{
    echo "$1" >> report-overwritten.txt
}

report-ignored()
{
    echo "$1" >> report-ignored.txt
}

report-error()
{
    echo "$1" >> report-error.txt
}

report-info()
{
    echo "$1" >> report-info.txt
}

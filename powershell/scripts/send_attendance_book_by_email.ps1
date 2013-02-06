# test or not
$test = $args[0] -eq "-test"
# Read email information contents information
$contents = Get-Content .\.attendance_book_email_info
# echo $contents

$emailFrom = $contents[0] # from@host.com
if ($test) {
    $emailTo = $contents[0] # to@host.com
} else {
    $emailTo = $contents[4] # to@host.com
}
$emailBcc = $emailFrom # bcc@host.com

$date = get-date -format d
$month = [string]([int](get-date -format MM))
$subject = "�o�Ε\�i�t�@�[�u���j " + $date
$smtpServerAddress = $contents[2] # smtp.host.com"
$smtpPort = [int]$contents[3] # 587 / 465
$date = $date -replace "/",""
$remote_filename = $contents[5] # attachement (remote)
$filename = $contents[6] # attachement (local copy)
cp $remote_filename $filename
$remote_sign = $contents[7] # sign (remote)
$sign = $contents[8] # sign (local copy)
cp $remote_sign $sign

$msg = New-Object Net.Mail.MailMessage
$att = New-Object Net.Mail.Attachment($filename)
$smtp = New-Object Net.Mail.SmtpClient($smtpServerAddress, $smtpPort)
$smtp.Timeout = 5000
$smtp.EnableSsl = $true
$password = $contents[1] # password
$credentials = New-Object System.Net.NetworkCredential($emailFrom, $password)
$smtp.UseDefaultCredentials = $false
$smtp.credentials = $credentials
[System.Net.ServicePointManager]::ServerCertificateValidationCallback = {$true} #Ignore untrusted certificates

$sign = [string]::join([environment]::newline,(get-content $sign))

$msg.From = $emailFrom
$msg.To.Add($emailTo)
$msg.Bcc.Add($emailBcc)
$msg.Subject = $subject
$body = "�쓈����A`r`n`r`n����" + $month + "�����̏o�Ε\�ł��B`r`n�X�������肢�v���܂��B`r`n`r`n�i�t�@�[�u���j`r`n`r`n`r`n�iPS�F���̃��b�Z�[�W��PowerShell�X�N���v�g�ɂ���Ď����I�ɐ������Ă��܂��j`r`n`r`n"
$msg.Body = $body + $sign
$msg.Attachments.Add($att)

#print a digest of the email to be sent
echo @"

#########################
# Attendance Book Email #
#########################

From:`t$emailFrom
To:`t$emailTo
Attachement:`t$filename (<- $remote_filename)

Subject:`t$subject
Body:
$body

Sign:
$sign

"@

$yes = New-Object System.Management.Automation.Host.ChoiceDescription "&Yes",""
$no = New-Object System.Management.Automation.Host.ChoiceDescription "&No",""
$choices = [System.Management.Automation.Host.ChoiceDescription[]]($yes,$no)
$result = $Host.UI.PromptForChoice(" < Confirmation > ", "Really send email sumarized in the digest above?", $choices, 0)

# If confirmed
if ($result -eq 0) {
    echo @"

####################
# SENDING EMAIL... #
####################

"@
    $smtp.Send($msg)
} else {
   echo "Aborted sending of the email..."
}

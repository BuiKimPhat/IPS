# Generated by Django 4.2.2 on 2023-06-06 08:34

from django.db import migrations, models
import monitorweb.utils.enums


class Migration(migrations.Migration):

    dependencies = [
        ('monitorweb', '0003_rule_operator_alter_rulecomponent_filter_field'),
    ]

    operations = [
        migrations.AlterField(
            model_name='rule',
            name='operator',
            field=models.CharField(choices=[(monitorweb.utils.enums.Operator['AND'], 'AND (&&)'), (monitorweb.utils.enums.Operator['OR'], 'OR (||)')], default='OR', max_length=10),
        ),
        migrations.AlterField(
            model_name='rulecomponent',
            name='filter_field',
            field=models.CharField(choices=[(monitorweb.utils.enums.Request['url'], 'Request (URL)'), (monitorweb.utils.enums.Request['status'], 'Status code'), (monitorweb.utils.enums.Request['bbs'], 'Body bytes sent'), (monitorweb.utils.enums.Request['user'], 'Remote user'), (monitorweb.utils.enums.Request['req_time'], 'Request time'), (monitorweb.utils.enums.Request['body'], 'Request body'), (monitorweb.utils.enums.Request['headers'], 'Request headers')], default='URL', max_length=30),
        ),
    ]

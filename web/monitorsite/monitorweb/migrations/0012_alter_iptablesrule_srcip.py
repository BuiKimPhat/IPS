# Generated by Django 4.2.1 on 2023-07-01 04:17

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('monitorweb', '0011_iptablesrule_options'),
    ]

    operations = [
        migrations.AlterField(
            model_name='iptablesrule',
            name='srcip',
            field=models.GenericIPAddressField(blank=True, null=True),
        ),
    ]

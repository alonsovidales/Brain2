#!/usr/bin/env python
import os

from brain import __version__

try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

setup(
    name='brain',
    version=__version__,
    description='Python client for Brain - On memory high performance distributed storage system',
    url='https://github.com/alonsovidales/Brain2',
    author='Alonso Vidales',
    author_email='alonso.vidales@tras2.es',
    maintainer='Alonso Vidales',
    maintainer_email='alonso.vidales@tras2.es',
    keywords=['Brain', 'key-value store', 'cache'],
    license='MIT',
    packages=['brain'],
    scripts=['brain-console.py'],
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2.5',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.2',
        'Programming Language :: Python :: 3.3',
    ]
)

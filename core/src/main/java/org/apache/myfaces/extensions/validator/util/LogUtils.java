/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.util;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.logging.Level;
import org.apache.myfaces.extensions.validator.core.logging.AbstractLoggerFactory;

/**
 * helper to get shorter logging statements
 *
 * @author Gerhard Petracek
 */
@UsageInformation(UsageCategory.INTERNAL)
public class LogUtils
{
    public static void trace(String message, Class targetClass)
    {
        trace(message, null, targetClass);
    }

    public static void trace(String message, Throwable throwable, Class targetClass)
    {
        ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.LOGGER_FACTORY, AbstractLoggerFactory.class)
            .create(targetClass).log(Level.TRACE, message, throwable);
    }

    public static void debug(String message, Class targetClass)
    {
        debug(message, null, targetClass);
    }

    public static void debug(String message, Throwable throwable, Class targetClass)
    {
        ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.LOGGER_FACTORY, AbstractLoggerFactory.class)
            .create(targetClass).log(Level.DEBUG, message, throwable);
    }

    public static void info(String message, Class targetClass)
    {
        info(message, null, targetClass);
    }

    public static void info(String message, Throwable throwable, Class targetClass)
    {
        ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.LOGGER_FACTORY, AbstractLoggerFactory.class)
            .create(targetClass).log(Level.INFO, message, throwable);
    }

    public static void warn(String message, Class targetClass)
    {
        warn(message, null, targetClass);
    }

    public static void warn(String message, Throwable throwable, Class targetClass)
    {
        ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.LOGGER_FACTORY, AbstractLoggerFactory.class)
            .create(targetClass).log(Level.WARN, message, throwable);
    }

    public static void error(String message, Class targetClass)
    {
        error(message, null, targetClass);
    }

    public static void error(String message, Throwable throwable, Class targetClass)
    {
        ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.LOGGER_FACTORY, AbstractLoggerFactory.class)
            .create(targetClass).log(Level.ERROR, message, throwable);
    }

    public static void fatal(String message, Class targetClass)
    {
        fatal(message, null, targetClass);
    }

    public static void fatal(String message, Throwable throwable, Class targetClass)
    {
        ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.LOGGER_FACTORY, AbstractLoggerFactory.class)
            .create(targetClass).log(Level.FATAL, message, throwable);
    }
}

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
package org.apache.myfaces.extensions.validator.core.logging;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class LoggerWrapper implements Logger
{
    private Logger logger;
    private static java.util.logging.Logger jdkLogger;

    public LoggerWrapper(Logger logger)
    {
        this.logger = logger;

        if(jdkLogger == null)
        {
            jdkLogger = java.util.logging.Logger.getLogger(LoggerWrapper.class.getName());

            //log this warning just once
            if(this.logger == null)
            {
                jdkLogger.logp(java.util.logging.Level.WARNING, LoggerWrapper.class.getName(), "LoggerWrapper()",
                "no logger attached - e.g.: for commons logging please use the commons-logging-support module");
            }
        }
    }

    public void log(Level level, String message)
    {
        log(level, message, null);
    }

    public void log(Level level, String message, Throwable throwable)
    {
        if(this.logger != null)
        {
            this.logger.log(level, message, throwable);
        }
    }
}
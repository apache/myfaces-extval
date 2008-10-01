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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.impl.Jdk14Logger;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class CommonsLoggingLoggerAdapter implements Logger
{
    private Log logger;

    public CommonsLoggingLoggerAdapter(Log logger)
    {
        if(!(logger instanceof Jdk14Logger))
        {
            this.logger = logger;
        }
        else
        {
            this.logger = new ExtValJdk14Logger(((Jdk14Logger)logger).getLogger().getName());
        }
    }

    public void log(Level level, String message)
    {
        log(level, message, null);
    }

    public void log(Level level, String message, Throwable throwable)
    {
        switch (level)
        {
            case TRACE:
                trace(message, throwable);
                break;

            case DEBUG:
                debug(message, throwable);
                break;

            case INFO:
                info(message, throwable);
                break;

            case WARN:
                warn(message, throwable);
                break;

            case ERROR:
                error(message, throwable);
                break;

            case FATAL:
                fatal(message, throwable);
                break;

            default:
        }
    }

    private void trace(String message, Throwable throwable)
    {
        if(this.logger.isTraceEnabled())
        {
            if(throwable == null)
            {
                this.logger.trace(message);
            }
            else
            {
                this.logger.trace(message, throwable);
            }
        }
    }

    private void debug(String message, Throwable throwable)
    {
        if(this.logger.isDebugEnabled())
        {
            if(throwable == null)
            {
                this.logger.debug(message);
            }
            else
            {
                this.logger.debug(message, throwable);
            }
        }
    }

    private void info(String message, Throwable throwable)
    {
        if(this.logger.isInfoEnabled())
        {
            if(throwable == null)
            {
                this.logger.info(message);
            }
            else
            {
                this.logger.info(message, throwable);
            }
        }
    }

    private void warn(String message, Throwable throwable)
    {
        if(this.logger.isWarnEnabled())
        {
            if(throwable == null)
            {
                this.logger.warn(message);
            }
            else
            {
                this.logger.warn(message, throwable);
            }
        }
    }

    private void error(String message, Throwable throwable)
    {
        if(this.logger.isErrorEnabled())
        {
            if(throwable == null)
            {
                this.logger.error(message);
            }
            else
            {
                this.logger.error(message, throwable);
            }
        }
    }

    private void fatal(String message, Throwable throwable)
    {
        if(this.logger.isFatalEnabled())
        {
            if(throwable == null)
            {
                this.logger.fatal(message);
            }
            else
            {
                this.logger.fatal(message, throwable);
            }
        }
    }
}
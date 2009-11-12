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
import org.apache.myfaces.extensions.validator.core.mapper.ClassMappingFactory;

/**
 * to inject a custom logger factory use:
 *         ExtValContext.getContext().getFactoryFinder()
 *          .getFactory(FactoryNames.LOGGER_FACTORY, AbstractLoggerFactory.class)
 *          .setCustomLoggerFactory(new CustomLoggerFactory());
 * e.g. via a startup-listener - see AbstractStartupListener
 * 
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public abstract class AbstractLoggerFactory implements ClassMappingFactory<Class, Logger>
{
    protected AbstractLoggerFactory customLoggerFactory;
    
    public void setCustomLoggerFactory(AbstractLoggerFactory loggerFactory)
    {
        this.customLoggerFactory = loggerFactory;
    }

    public final Logger create(Class targetClass)
    {
        Logger result = null;

        if(this.customLoggerFactory != null)
        {
            result = this.customLoggerFactory.createLogger(targetClass);
        }

        if(result == null)
        {
            return createLogger(targetClass);
        }

        return result;
    }

    protected abstract Logger createLogger(Class targetClass);
}
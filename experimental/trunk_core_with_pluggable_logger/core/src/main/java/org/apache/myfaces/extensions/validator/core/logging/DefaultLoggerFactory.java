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
import org.apache.myfaces.extensions.validator.util.ELUtils;

import javax.faces.context.FacesContext;
import java.util.Map;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultLoggerFactory extends AbstractLoggerFactory
{
    private Map<Class, Logger> classToLoggerMapping = new HashMap<Class, Logger>();

    protected Logger createLogger(Class targetClass)
    {
        Logger customLogger = null;

        if(FacesContext.getCurrentInstance() != null)
        {
            customLogger = (Logger) ELUtils.getValueOfExpression(FacesContext.getCurrentInstance(),
                    "#{customExtValLoggerAdapter}");
        }

        if(!this.classToLoggerMapping.containsKey(targetClass))
        {
            this.classToLoggerMapping.put(targetClass, customLogger);
        }
        return new LoggerWrapper(customLogger);
    }
}
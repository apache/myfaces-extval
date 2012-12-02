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
package org.apache.myfaces.extensions.validator.core.mapper;

import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.logging.Logger;
/**
 * A generic implementation of a custom name mapper that can be configured using configuration. Subclasses just have
 * to specify the fully qualified name of the configured name mapper.
 *
 * NameMappers are stateless.
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.REUSE)
public abstract class AbstractCustomNameMapper<T> implements NameMapper<T>
{
    protected final Logger logger = Logger.getLogger(getClass().getName());
    private NameMapper<T> customNameMapper;

    protected AbstractCustomNameMapper()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    /**
     * {@inheritDoc}
     * When no name mapper is configured, the method returns null.
     */
    public String createName(T source)
    {
        if (customNameMapper == null)
        {
            String className = getCustomNameMapperClassName();

            if (className != null)
            {
                customNameMapper = (NameMapper<T>) ClassUtils.tryToInstantiateClassForName(className);
            }
        }

        return (customNameMapper != null) ? customNameMapper.createName(source) : null;
    }

    /**
     * Returns the fully qualified class name of the configured name mapper that must be used.
     *
     * @return fully qualified class name of a custom name mapper.
     */
    protected abstract String getCustomNameMapperClassName();
}

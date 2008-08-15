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
package org.apache.myfaces.extensions.validator.core.adapter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.core.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.adapter.mapper.CustomConfiguredConverterToAdapterNameMapper;
import org.apache.myfaces.extensions.validator.core.adapter.mapper.CustomConventionConverterToAdapterNameMapper;
import org.apache.myfaces.extensions.validator.core.adapter.mapper.DefaultConverterToAdapterNameMapper;
import org.apache.myfaces.extensions.validator.core.adapter.mapper.SimpleConverterToAdapterNameMapper;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.util.ClassUtils;

import javax.faces.convert.Converter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * just for the fallback solution
 *
 * @author Gerhard Petracek
 */
@Deprecated
public class DefaultConverterAdapterFactory implements
        ClassMappingFactory<Converter, Converter>
{
    private static Map<String, String> converterAdapterMapping = new HashMap<String, String>();
    private static List<NameMapper<Converter>> nameMapperList = new ArrayList<NameMapper<Converter>>();

    static
    {
        nameMapperList.add(new CustomConfiguredConverterToAdapterNameMapper());
        nameMapperList.add(new CustomConventionConverterToAdapterNameMapper());
        nameMapperList.add(new DefaultConverterToAdapterNameMapper());
        nameMapperList.add(new SimpleConverterToAdapterNameMapper());
    }

    protected final Log logger = LogFactory.getLog(getClass());

    public Converter create(Converter converter)
    {
        String converterName = converter.getClass().getName();

        if (converterAdapterMapping.containsKey(converterName))
        {
            return (Converter) ClassUtils
                    .tryToInstantiateClassForName(converterAdapterMapping
                            .get(converterName));
        }

        Converter adapter;
        String adapterName;

        for (NameMapper<Converter> nameMapper : nameMapperList)
        {
            adapterName = nameMapper.createName(converter);
            adapter = (Converter) ClassUtils
                    .tryToInstantiateClassForName(adapterName);

            if (adapter != null)
            {
                addMapping(converterName, adapter.getClass().getName());

                if (this.logger.isTraceEnabled())
                {
                    this.logger.trace("used adapter: " + adapter.getClass().getName());
                }

                return adapter;
            }
        }

        if (this.logger.isDebugEnabled())
        {
            this.logger.debug("no adapter found for " + converterName 
                    + " -> converter itself is used -> no sev-en support");
        }
        return converter;
    }

    private void addMapping(String sourceConverter, String adapter)
    {
        synchronized (DefaultConverterAdapterFactory.class)
        {
            converterAdapterMapping.put(sourceConverter, adapter);
        }
        //TODO logging
    }
}

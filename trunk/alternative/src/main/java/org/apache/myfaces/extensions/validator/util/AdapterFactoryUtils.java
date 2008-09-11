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

import org.apache.myfaces.extensions.validator.core.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.adapter.DefaultConverterAdapterFactory;

import javax.faces.convert.Converter;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Gerhard Petracek
 */
public class AdapterFactoryUtils
{
    private static ClassMappingFactory<Converter, Converter> converterAdapterFactory;

    @Deprecated
    public static ClassMappingFactory<Converter, Converter> getConverterAdapterFactory()
    {
        if (converterAdapterFactory == null)
        {
            List<String> converterAdapterFactoryClassNames = new ArrayList<String>();

            converterAdapterFactoryClassNames
                .add(WebXmlParameter.CUSTOM_CONVERTER_ADAPTER_FACTORY);
            converterAdapterFactoryClassNames.add(ExtValUtils
                .getInformationProviderBean()
                .getCustomConverterAdapterFactory());
            converterAdapterFactoryClassNames
                .add(DefaultConverterAdapterFactory.class.getName());

            for (String className : converterAdapterFactoryClassNames)
            {
                converterAdapterFactory = (ClassMappingFactory<Converter, Converter>) ClassUtils
                    .tryToInstantiateClassForName(className);

                if (converterAdapterFactory != null)
                {
                    //TODO logging
                    break;
                }
            }
        }
        return converterAdapterFactory;
    }
}

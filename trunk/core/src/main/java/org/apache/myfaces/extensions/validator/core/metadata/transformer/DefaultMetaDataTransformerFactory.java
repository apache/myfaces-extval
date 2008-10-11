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
package org.apache.myfaces.extensions.validator.core.metadata.transformer;

import org.apache.myfaces.extensions.validator.core.mapper.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.strategy.BeanValidationStrategyAdapter;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper
        .CustomConfiguredValidationStrategyToMetaDataTransformerNameMapper;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper
        .CustomConventionValidationStrategyToMetaDataTransformerNameMapper;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper
        .DefaultValidationStrategyToMetaDataTransformerNameMapper;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper
        .SimpleValidationStrategyToMetaDataTransformerNameMapper;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper
        .BeanValidationStrategyToMetaDataTransformerNameMapper;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.loader.StaticMappingConfigLoader;
import org.apache.myfaces.extensions.validator.core.loader.StaticMappingConfigLoaderNames;
import org.apache.myfaces.extensions.validator.core.loader.StaticMappingConfigEntry;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * Factory which creates the MetaDataTransformer for a given ValidationStrategy
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
/*
 * ValidationStrategy -> MetaDataTransformer instead of Meta-Data -> MetaDataTransformer
 * to avoid a second static mapping e.g. for jpa annotations
 */
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.CUSTOMIZABLE})
public class DefaultMetaDataTransformerFactory implements
    ClassMappingFactory<ValidationStrategy, MetaDataTransformer>
{
    protected final Log logger = LogFactory.getLog(getClass());

    private static Map<String, String> validationStrategyToMetaDataTransformerMapping;
    private static List<NameMapper<ValidationStrategy>> nameMapperList
        = new ArrayList<NameMapper<ValidationStrategy>>();

    static
    {
        nameMapperList
            .add(new CustomConfiguredValidationStrategyToMetaDataTransformerNameMapper());
        nameMapperList
            .add(new CustomConventionValidationStrategyToMetaDataTransformerNameMapper());
        nameMapperList
            .add(new DefaultValidationStrategyToMetaDataTransformerNameMapper());
        nameMapperList
            .add(new SimpleValidationStrategyToMetaDataTransformerNameMapper());
        nameMapperList
            .add(new BeanValidationStrategyToMetaDataTransformerNameMapper());
    }

    public DefaultMetaDataTransformerFactory()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public MetaDataTransformer create(ValidationStrategy validationStrategy)
    {
        String validationStrategyName = null;

        //proxy check
        if(validationStrategy.getClass().getPackage() != null)
        {
            validationStrategyName = validationStrategy.getClass().getName();
        }
        //in case of a proxy and the usage of a BeanValidationStrategyAdapter
        else if (validationStrategy instanceof BeanValidationStrategyAdapter)
        {
            validationStrategyName = ((BeanValidationStrategyAdapter)validationStrategy)
                                        .getValidationStrategyClassName();
        }

        if (validationStrategyToMetaDataTransformerMapping == null)
        {
            initStaticMappings();
        }

        if (validationStrategyToMetaDataTransformerMapping.containsKey(validationStrategyName))
        {
            return (MetaDataTransformer)ClassUtils.tryToInstantiateClassForName(
                validationStrategyToMetaDataTransformerMapping.get(validationStrategyName));
        }

        MetaDataTransformer metaDataTransformer;
        String transformerName;
        //null -> use name mappers
        for (NameMapper<ValidationStrategy> nameMapper : nameMapperList)
        {
            transformerName = nameMapper.createName(validationStrategy);

            if (transformerName == null)
            {
                continue;
            }

            metaDataTransformer = (MetaDataTransformer)ClassUtils.tryToInstantiateClassForName(transformerName);

            if (metaDataTransformer != null)
            {
                if(validationStrategyName != null)
                {
                    addMapping(validationStrategyName, transformerName);
                }
                return metaDataTransformer;
            }
        }

        return null;
    }

    private void initStaticMappings()
    {
        synchronized (DefaultMetaDataTransformerFactory.class)
        {
            validationStrategyToMetaDataTransformerMapping = new HashMap<String, String>();

            //setup internal static mappings
            for (StaticMappingConfigLoader<String, String> staticMappingConfigLoader :
                ExtValContext.getContext().getStaticMappingConfigLoaders(
                    StaticMappingConfigLoaderNames.VALIDATION_STRATEGY_TO_META_DATA_TRANSFORMER_CONFIG_LOADER))
            {
                setupStrategyMappings(staticMappingConfigLoader.getMapping());
            }
        }
    }

    private void setupStrategyMappings(List<StaticMappingConfigEntry<String, String>> mappings)
    {
        for(StaticMappingConfigEntry<String, String> mapping : mappings)
        {
            addMapping(mapping.getSource(), mapping.getTarget());
        }
    }

    private void addMapping(String validationStrategyName, String transformerName)
    {
        if(logger.isTraceEnabled())
        {
            logger.trace("adding validation strategy to meta-data transformer mapping: "
                + validationStrategyName + " -> " + transformerName);
        }

        synchronized (DefaultMetaDataTransformerFactory.class)
        {
            validationStrategyToMetaDataTransformerMapping.put(validationStrategyName, transformerName);
        }
    }
}
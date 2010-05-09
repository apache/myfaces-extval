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

import org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.factory.AbstractNameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.strategy.BeanValidationStrategyAdapter;
import org.apache.myfaces.extensions.validator.core.validation.strategy.IdentifiableValidationStrategy;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.mapper.SubMapperAwareNameMapper;
import org.apache.myfaces.extensions.validator.core.Nested;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationEntry;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper.
        ValidationStrategyToMetaDataTransformerSubMapperAwareNameMapper;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;


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
public class DefaultMetaDataTransformerFactory extends AbstractNameMapperAwareFactory<ValidationStrategy>
        implements ClassMappingFactory<ValidationStrategy, MetaDataTransformer>
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private Map<String, String> validationStrategyToMetaDataTransformerMapping;
    private List<NameMapper<ValidationStrategy>> nameMapperList = new ArrayList<NameMapper<ValidationStrategy>>();
    private List<NameMapper<ValidationStrategy>> subNameMapperList =
            new ArrayList<NameMapper<ValidationStrategy>>();

    public DefaultMetaDataTransformerFactory()
    {
        logger.fine(getClass().getName() + " instantiated");

        //since there is no guarantee that the startup listener of the core gets executed first
        register(new ValidationStrategyToMetaDataTransformerSubMapperAwareNameMapper());
    }

    public MetaDataTransformer create(ValidationStrategy validationStrategy)
    {
        String validationStrategyName = createValidationStrategyName(validationStrategy);

        tryToInitStaticMappings();

        MetaDataTransformer metaDataTransformer =
                tryToResolveCachedMetaDataTransformer(validationStrategy, validationStrategyName);

        if(metaDataTransformer != null)
        {
            return metaDataTransformer;
        }

        return createAndCacheMetaDataTransformer(validationStrategy, validationStrategyName);
    }

    private String createValidationStrategyName(ValidationStrategy validationStrategy)
    {
        boolean isProxyDetected = isProxy(validationStrategy);
        //in case of a proxy and the usage of a BeanValidationStrategyAdapter
        if (isProxyDetected && validationStrategy instanceof BeanValidationStrategyAdapter)
        {
            return ((BeanValidationStrategyAdapter)validationStrategy)
                                        .getValidationStrategyClassName();
        }

        return !isProxyDetected ? ProxyUtils.getClassName(validationStrategy.getClass()) : null;
    }

    private void tryToInitStaticMappings()
    {
        if (validationStrategyToMetaDataTransformerMapping == null)
        {
            initStaticMappings();
        }
    }

    private boolean isProxy(ValidationStrategy validationStrategy)
    {
        return validationStrategy.getClass().getPackage() == null;
    }

    private MetaDataTransformer createAndCacheMetaDataTransformer(
            ValidationStrategy validationStrategy, String validationStrategyName)
    {
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

            metaDataTransformer = tryToCreateAndCacheMetaDataTransformer(
                    validationStrategy, validationStrategyName, transformerName);

            if(metaDataTransformer != null)
            {
                return metaDataTransformer;
            }
        }

        return null;
    }

    private MetaDataTransformer tryToResolveCachedMetaDataTransformer(
            ValidationStrategy validationStrategy, String validationStrategyName)
    {
        if (validationStrategyToMetaDataTransformerMapping.containsKey(validationStrategyName))
        {
            return (MetaDataTransformer)ClassUtils.tryToInstantiateClassForName(
                validationStrategyToMetaDataTransformerMapping.get(validationStrategyName));
        }

        if(validationStrategy instanceof IdentifiableValidationStrategy)
        {
            String newValidationStrategyName = validationStrategyName + IdentifiableValidationStrategy.ID_PREFIX +
                    ((IdentifiableValidationStrategy)validationStrategy).getId();

            if (validationStrategyToMetaDataTransformerMapping.containsKey(newValidationStrategyName))
            {
                return (MetaDataTransformer)ClassUtils.tryToInstantiateClassForName(
                    validationStrategyToMetaDataTransformerMapping.get(newValidationStrategyName));
            }
        }

        return null;
    }

    private MetaDataTransformer tryToCreateAndCacheMetaDataTransformer(
            ValidationStrategy validationStrategy, String validationStrategyName, String transformerName)
    {
        MetaDataTransformer metaDataTransformer = (MetaDataTransformer)
                ClassUtils.tryToInstantiateClassForName(transformerName);

        if (metaDataTransformer != null)
        {
            if(validationStrategyName != null)
            {
                if(validationStrategy instanceof IdentifiableValidationStrategy)
                {
                    validationStrategyName += IdentifiableValidationStrategy.ID_PREFIX +
                            ((IdentifiableValidationStrategy)validationStrategy).getId();
                }
                addMapping(validationStrategyName, transformerName);
            }
            return metaDataTransformer;
        }

        return null;
    }

    private synchronized void initStaticMappings()
    {
        validationStrategyToMetaDataTransformerMapping = new HashMap<String, String>();

        //setup internal static mappings
        for (StaticConfiguration<String, String> staticConfig :
            ExtValContext.getContext().getStaticConfiguration(
                StaticConfigurationNames.VALIDATION_STRATEGY_TO_META_DATA_TRANSFORMER_CONFIG))
        {
            setupMappings(staticConfig.getMapping());
        }
    }

    private void setupMappings(List<StaticConfigurationEntry<String, String>> mappings)
    {
        for(StaticConfigurationEntry<String, String> mapping : mappings)
        {
            addMapping(mapping.getSource(), mapping.getTarget());
        }
    }

    private synchronized void addMapping(String validationStrategyName, String transformerName)
    {
        logger.finest("adding validation strategy to meta-data transformer mapping: "
            + validationStrategyName + " -> " + transformerName);

        validationStrategyToMetaDataTransformerMapping.put(validationStrategyName, transformerName);
    }

    protected List<NameMapper<ValidationStrategy>> getNameMapperList()
    {
        return new SortedNameMapperList<NameMapper<ValidationStrategy>>(this.nameMapperList, this.subNameMapperList);
    }

    @Override
    public void register(NameMapper<ValidationStrategy> validationStrategyNameMapper)
    {
        tryToInitNameMapperWithExistingSubMappers(validationStrategyNameMapper);
        super.register(validationStrategyNameMapper);
    }

    /**
     * it's a very special case due to the missing order in the execution of startup-listeners (phase listeners)
     * packaged in faces-config.xml files of jars
     *
     * normally the default SubMapperAwareNameMapper should be enough
     * anyway, if a module adds a new SubMapperAwareNameMapper,
     * all previous added SubNameMappers have to be added to avoid confusion in special cases.
     * if a SubMapperAwareNameMapper should be considered as final extend the interface and filter it in addNameMapper
     * 
     * @param validationStrategyNameMapper which has to be added
     */
    private void tryToInitNameMapperWithExistingSubMappers(NameMapper<ValidationStrategy> validationStrategyNameMapper)
    {
        if(validationStrategyNameMapper instanceof SubMapperAwareNameMapper)
        {
            for(NameMapper<ValidationStrategy> nameMapper : this.subNameMapperList)
            {
                if(nameMapper.getClass().isAnnotationPresent(Nested.class))
                {
                    ((SubMapperAwareNameMapper<ValidationStrategy>)validationStrategyNameMapper)
                            .addNameMapper(nameMapper);
                }
            }
        }
    }
}
